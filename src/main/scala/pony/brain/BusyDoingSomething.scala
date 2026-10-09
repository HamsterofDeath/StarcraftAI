package pony
package brain

import scala.util.control.NonFatal

class BusyDoingSomething[T <: WrapsUnit](
    employer: Employer[T],
    behaviour: Seq[SingleUnitBehaviour[T]],
    private var objective: Objective
) extends UnitWithJob(employer, behaviour.head.unit, Priority.DefaultBehaviour) with Interruptable[T] {

  assert(behaviour.map(_.unit).distinct.size == 1, s"Wrong grouping: $behaviour")

  private var lastTickOrderIssuedBy = Option.empty[SingleUnitBehaviour[T]]
  private var lastOrderIssuedBy     = Option.empty[SingleUnitBehaviour[T]]

  override def onStealUnit() = {
    super.onStealUnit()
    behaviour.foreach(_.onStealUnit())
  }

  override def isNoopJob = {
    lastOrderIssuedBy.exists(_.isNoopTask)
  }

  override def interruptableNow = super.interruptableNow &&
    lastTickOrderIssuedBy.exists(_.canInterrupt)

  def newObjective_!(objective: Objective): Unit = {
    this.objective = objective
  }

  override def shortDebugString = {
    val realOrder = lastTickOrderIssuedBy.map(_.describeShort).getOrElse("???")
    val lastOrder = lastOrderIssuedBy.map(_.describeShort).getOrElse("???")
    s"[BG] $realOrder ($lastOrder)"
  }

  override def renderDebug(renderer: Renderer) = {
    super.renderDebug(renderer)
    lastOrderIssuedBy.foreach(_.renderDebug(renderer))
  }

  // never ends
  override def isFinished = false

  override def ordersForTick = {
    val tmp = highestPriorityOrdersForTick
    // keep track of it for debugging purposes
    lastTickOrderIssuedBy = tmp._1
    lastOrderIssuedBy = lastTickOrderIssuedBy.orElse(lastOrderIssuedBy)

    tmp._2
  }

  private def highestPriorityOrdersForTick = {
    val options = active.map { rule =>
      rule -> CpuProfile.time("rule:" + BusyDoingSomething.ruleName(rule)) {
        try rule.orderForTick(objective).map(_.lockingFor_!(rule.blocksForTicks).forceRepeat_!(rule.forceRepeats))
        catch {
          case NonFatal(failure) =>
            BusyDoingSomething.failed(rule, failure)
            Nil
        }
      }
    }.filter(_._2.nonEmpty)
    if (options.isEmpty) {
      None -> Nil
    } else {
      val (a, b) = options.maxBy(_._1.priority)
      Some(a) -> b
    }
  }

  private def active = behaviour.filter(rule =>
    CpuProfile.time("precondition:" + BusyDoingSomething.ruleName(rule))(rule.preconditionOk)
  )

  override protected def omitRepeatedOrders = true

}

object BusyDoingSomething {

  private val failures = scala.collection.mutable.HashMap.empty[String, Int]

  /**
    * One behaviour failing for one unit must not end the game: the rule gives no order this frame, and its first
    * failures are traced.
    */
  def failed(rule: AnyRef, failure: Throwable): Unit = {
    val name  = ruleName(rule)
    val count = failures.getOrElse(name, 0) + 1
    failures(name) = count
    if (count <= 3 || count % 1000 == 0)
      NativeMatchEvidence.trace(
        "rule-failure",
        s"rule=$name count=$count failure=$failure at=${failure.getStackTrace.take(3).mkString(" < ")}"
      )
  }

  /** The behaviour a rule belongs to, without the anonymous class suffix. */
  def ruleName(rule: AnyRef): String = rule.getClass.getName.split('.').last.replaceAll("[$][$]anon[$][0-9]+", "")
}
