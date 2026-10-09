package pony
package brain

case class AnyUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[_ <: T], amount: Int)
  extends UnitRequest[T] {
  override def priority: Priority = Priority.Default

}
