package pony
package brain

trait Orderless[T <: WrapsUnit] extends AIModule[T] {
  override def ordersForTick: Iterable[UnitOrder] = {
    onTick_!()
    Nil
  }

  def onTick_!(): Unit
}
