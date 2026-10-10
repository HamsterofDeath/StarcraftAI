package pony
package brain

import pony.brain.jobs.Employer
import pony.render.Renderer
import pony.units.WrapsUnit

import scala.reflect.ClassTag

abstract class AIModule[T <: WrapsUnit: ClassTag](override val universe: Universe)
    extends Employer[T](universe) with HasUniverse {
  def ordersForTick: Iterable[UnitOrder]

  def onNth: Int = 1

  override def toString = s"Module ${getClass.className}"

  def renderDebug(renderer: Renderer): Unit = {}
}

object AIModule {
  def noop[T <: WrapsUnit: ClassTag](universe: Universe) = new AIModule[T](universe) {
    override def ordersForTick: Iterable[UnitOrder] = Nil
  }
}
