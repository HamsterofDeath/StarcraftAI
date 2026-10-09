package pony
package brain

trait CanAcceptUnitSwitch[T <: WrapsUnit] extends UnitWithJob[T] {
  def stillWantsOptimization = !failedOrObsolete
  def copyOfJobForNewUnit(replacement: T): UnitWithJob[T]
  def asRequest: UnitJobRequest[T]
  def canSwitchNow: Boolean
  def couldSwitchInTheFuture: Boolean
  def hasToSwitchLater = !canSwitchNow && couldSwitchInTheFuture
}
