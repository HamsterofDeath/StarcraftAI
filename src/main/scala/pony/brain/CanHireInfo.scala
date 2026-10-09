package pony
package brain

case class CanHireInfo[T <: WrapsUnit](request: Option[UnitJobRequest[T]], details: Set[T])

object CanHireInfo {
  def empty[T <: WrapsUnit] = CanHireInfo[T](None, Set.empty)
}
