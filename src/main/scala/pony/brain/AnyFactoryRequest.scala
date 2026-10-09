package pony
package brain

case class AnyFactoryRequest[T <: UnitFactory, U <: Mobile](typeOfRequestedUnit: Class[? <: T],
                                                            amount: Int,
                                                            buildThis: Class[? <: U])
  extends UnitRequest[T] {
  override def acceptable(unit: T): Boolean = {
    super.acceptable(unit) && unit.canBuild(buildThis)
  }

  override def priority: Priority = Priority.Default
}
