package pony
package brain

case class SpecificUnitRequest[T <: WrapsUnit](unit: T) extends UnitRequest[T] {
  override def typeOfRequestedUnit = unit.getClass

  override def amount: Int = 1

  override def priority: Priority = Priority.Default
}
