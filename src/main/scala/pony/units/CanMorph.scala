package pony
package units

trait CanMorph extends WrapsUnit {
  override def shouldReRegisterOnMorph = true
}
