package pony
package brain

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.reflect.ManifestFactory

trait BackgroundComputation[T <: WrapsUnit] extends AIModule[T] {
  type ComputationInput

  private var backgroundOp           = Future
                                       .successful(BackgroundComputationResult.nothing[T](() => {}))
  private var currentResult          = Option.empty[BackgroundComputationResult[T]]
  private var waitingForBackgroundOp = false

  override def ordersForTick: Traversable[UnitOrder] = {
    currentResult.filter(_.repeatOrderIssue) match {
      // reuse current computation result as long as it is valid
      case Some(result) => result.orders
      case None =>
        // kick old result
        currentResult = None
        if (waitingForBackgroundOp && !backgroundOp.isCompleted) {
          // waiting, but no result yet: noop
          Nil
        } else if (waitingForBackgroundOp && backgroundOp.isCompleted) {
          // we were waiting for a computation to finish
          val computationResult = Await.result(backgroundOp, Duration.Zero)
          computationResult.afterComputation()
          info(s"Background computation finished, result is $computationResult")
          currentResult = Some(computationResult)
          waitingForBackgroundOp = false
          computationResult.jobs.foreach {
            case switch: CanAcceptUnitSwitch[T] =>
              // time has passed, pick a new unit for this job if possible
              val req = switch.asRequest
              implicit val summonedManifest: Manifest[T] =
                ManifestFactory.classType(switch.unit.getClass)
              val candidates = {
                def recycle = {
                  Set(switch: UnitWithJob[T])
                  .filter(job => unitManager.jobOptOf(switch.unit).contains(job))
                }
                unitManager.requestWithoutTracking[T](req, recycle)
              }
              var assigned = false
              candidates.headOption.foreach { replacement =>
                if (switch.failedOrObsolete) {
                  warn(s"Construction job failed while calculations were ongoing")
                } else {
                  if (replacement != switch.unit) {
                    val newRequest = switch.copyOfJobForNewUnit(replacement)
                    trace(s"Unit ${switch.unit} replaced by $replacement")
                    assignJob_!(newRequest)
                    assigned = true
                  } else {
                    trace(s"Unit ${switch.unit} kept its job after a background calculation",
                      replacement == switch.unit)
                    assignJob_!(switch)
                    assigned = true
                  }
                }
              }
              warn(
                s"Background calculation finished, but no unit could do the job anymore: $switch",
                candidates.isEmpty)
              if (!assigned) {
                switch.fail_!()
                // Unassigned jobs will never reach UnitManager's normal terminal listeners.
                if (!unitManager.jobOptOf(switch.unit).contains(switch)) switch.onFinishOrFail()
              }
            case job =>
              trace(s"Unit ${job.unit} kept its job after a background calculation")
              assignJob_!(job)
          }
          Nil
        } else {
          // we are not waiting
          calculationInput match {
            case None => Nil
            case Some(in) =>
              info(s"Background computation starting, input is $in")
              backgroundOp = Future {evaluateNextOrders(in)}
              waitingForBackgroundOp = true
              Nil
          }
        }
    }
  }

  def calculationInput: Option[ComputationInput]

  /**
    * do not access the universe here, it's running in another thread!
    */
  def evaluateNextOrders(in: ComputationInput): BackgroundComputationResult[T]
}
