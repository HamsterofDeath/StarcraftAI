package pony
package brain

import bwapi.Order

class BusyDoingNothing[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.None) with IssueOrderNTimes[T] with Interruptable[T] {
  override def isIdle = true

  override def getOrder: Seq[UnitOrder] = {
    unit match {
      case m: Mobile if m.currentOrder != Order.PlayerGuard && m.currentOrder != Order.Stop =>
        Orders.Stop(m).toSeq
      case _ => Nil
    }
  }

  override def isNoopJob = true

  override def times: Int = 5

  override def isFinished = false

  override def shortDebugString: String = "Idle"
}
