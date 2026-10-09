package pony
package brain

trait Orderless[T <: WrapsUnit] extends AIModule[T] {
  override def ordersForTick: Traversable[UnitOrder] = {
    onTick_!()
    Nil
  }

  def onTick_!(): Unit
}
