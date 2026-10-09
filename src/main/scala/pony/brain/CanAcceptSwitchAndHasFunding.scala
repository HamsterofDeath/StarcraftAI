package pony
package brain

trait CanAcceptSwitchAndHasFunding[T <: WrapsUnit]
    extends JobHasFunding[T] with CanAcceptUnitSwitch[T] {
  override def onStealUnit(): Unit = {
    super.onStealUnit()
  }
}
