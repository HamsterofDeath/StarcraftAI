package pony
package brain

trait JobOrSubJob[+T <: WrapsUnit] extends HasUniverse {
  def unit: T

  def renderDebug(renderer: Renderer) = {}

  protected def higherPriorityOrder = Seq.empty[UnitOrder]

}
