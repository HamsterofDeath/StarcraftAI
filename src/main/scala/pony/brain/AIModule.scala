package pony
package brain

abstract class AIModule[T <: WrapsUnit : Manifest](override val universe: Universe)
  extends Employer[T](universe) with HasUniverse {
  def ordersForTick: Traversable[UnitOrder]

  def onNth: Int = 1

  override def toString = s"Module ${getClass.className}"

  def renderDebug(renderer: Renderer): Unit = {}
}

object AIModule {
  def noop[T <: WrapsUnit : Manifest](universe: Universe) = new AIModule[T](universe) {
    override def ordersForTick: Traversable[UnitOrder] = Nil
  }
}
