package pony
package brain
package jobs

import pony.units.{CanDie, OrderHistorySupport, WrapsUnit}

import bwapi.Order

import scala.collection.mutable.ArrayBuffer

abstract class UnitWithJob[T <: WrapsUnit](
    val employer: Employer[T],
    val unit: T,
    val priority: Priority
) extends JobOrSubJob[T] {

  override val universe     = employer.universe
  private val myJobId       = JobCounter.next()
  private val realStartTick = currentTick
  private val listeners     = ArrayBuffer.empty[JobFinishedListener[T]]
  private var forceFail     = false
  private var obsolete      = false
  private var startTick     = currentTick

  unit match {
    case cd: CanDie if cd.isDead => fail_!()
    case _                       =>
  }
  private var noCommandsForTicks            = 0
  private var dead                          = false
  private var lastOrder: Seq[UnitOrder]     = Nil
  private var lastNonInterruptedOrderGiven  = Option.empty[Int]
  private var firstNonInterruptedOrderGiven = Option.empty[Int]

  ownUnits.registerKill_!(OnKillListener.on(
    unit,
    () => {
      trace(s"Unit $unit died, aborting $this")
      dead = true
    }
  ))

  def resetTimer_!(): Unit = {
    startTick = 0
  }

  def hasNotYetSpendResources: Boolean = true

  override def onTick_!(): Unit = {
    super.onTick_!()
  }

  def onStealUnit() = {}

  def shortDebugString: String

  def ageSinceLastReset = currentTick - startTick

  def age = currentTick - realStartTick

  def isIdle: Boolean = false

  /** Idle, or only following the always-on default behaviours: free to take on real work. */
  def isIdleOrDefault: Boolean = isIdle || this.isInstanceOf[BusyDoingSomething[?]]

  def lastIssuedOrder = lastOrder

  def wasInterceptedLastTick = ageSinceLastNonInterceptedOrder.fold(true)(_ > 1)

  def ageSinceLastNonInterceptedOrder = lastNonInterruptedOrderGiven.map(currentTick - _)

  def ageSinceFirstNonInterceptedOrder = firstNonInterruptedOrderGiven.map(currentTick - _)

  def ordersForThisTick = {
    if (failedOrObsolete) {
      Nil
    } else {
      if (noCommandsForTicks > 0) {
        noCommandsForTicks -= 1
        Nil
      } else {

        noCommandsForTicks = everyNth
        var sourceIsOriginal = false
        def injectedOrMine   = {
          val maybe = higherPriorityOrder
          if (maybe.isEmpty) {
            sourceIsOriginal = true
            ordersForTick
          } else
            maybe
        }
        val nextOrder = injectedOrMine

        if (sourceIsOriginal) {
          lastNonInterruptedOrderGiven = Some(currentTick)
          firstNonInterruptedOrderGiven.forNone {
            firstNonInterruptedOrderGiven = lastNonInterruptedOrderGiven
          }
        }

        val shouldRepeat = !nextOrder.exists(_.forceRepetition) &&
          omitRepeatedOrders &&
          nextOrder == lastOrder &&
          !unit.isDoingNothing
        if (shouldRepeat) {
          Nil
        } else {
          lastOrder = nextOrder
          nextOrder
        }
      }
    }
  }

  protected def omitRepeatedOrders = false

  def everyNth = 0

  def noCommandsForTicks_!(n: Int): Unit = {
    noCommandsForTicks = n
  }

  override def toString: String = s"[J#$myJobId] ${getClass.className} of $unit of $employer"

  def isFinished: Boolean

  def onFinishOrFail(): Unit = {
    listeners.foreach(_.onFinishOrFail(failedOrObsolete))
  }

  def failedOrObsolete = dead || forceFail || obsolete || jobHasFailedWithoutDeath

  def failureDebug = s"dead=$dead forceFail=$forceFail obsolete=$obsolete withoutDeath=$jobHasFailedWithoutDeath"

  private val inactive = Set(Order.PlayerGuard)

  def jobHasFailedWithoutDeath: Boolean = {
    unit match {
      case ohs: OrderHistorySupport if !isNoopJob => {
        // Only count frames this job was responsible for: stale history from earlier jobs
        // (or from before the hire) must not fail a fresh job before it can command its unit.
        val ownRecent              = ohs.unitHistory.take(24).takeWhile(e => e.job eq this).toVector
        val failedForUnknownReason = ownRecent.size >= 24 && ownRecent.forall(e => inactive(e.order))
        failedForUnknownReason
      }
      case _ => false
    }
  }

  def isNoopJob = false

  def listen_!(listener: JobFinishedListener[T]): Unit = listeners += listener

  def markObsolete_!(): Unit = {
    obsolete = true
  }

  def fail_!() = {
    forceFail = true
  }

  protected def ordersForTick: Seq[UnitOrder]
}
