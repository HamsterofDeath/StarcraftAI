package pony
package brain

class BusyBeingContructed[T <: WrapsUnit](unit: T, employer: Employer[T])
    extends UnitWithJob(employer, unit, Priority.Max) {
  override def isIdle = false

  override def ordersForTick: Seq[UnitOrder] = Nil

  override def isFinished = unit.nativeUnit.getRemainingBuildTime == 0

  override def shortDebugString: String = "Build me"
}
