package pony

trait CanMorph extends WrapsUnit {
  override def shouldReRegisterOnMorph = true
}
