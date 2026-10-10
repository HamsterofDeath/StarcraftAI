package pony
package brain
package jobs

import pony.render.Renderer
import pony.units.WrapsUnit

trait JobOrSubJob[+T <: WrapsUnit] extends HasUniverse {
  def unit: T

  def renderDebug(renderer: Renderer) = {}

  protected def higherPriorityOrder = Seq.empty[UnitOrder]

}
