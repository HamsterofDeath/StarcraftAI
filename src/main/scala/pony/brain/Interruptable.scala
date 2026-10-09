package pony
package brain

trait Interruptable[T <: WrapsUnit] extends UnitWithJob[T] {
  def interruptableNow = unit match {
    case gu: GroundUnit => gu.onGround
    case _              => true
  }
}
