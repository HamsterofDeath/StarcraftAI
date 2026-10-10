package pony
package brain
package jobs

import pony.units.{GroundUnit, WrapsUnit}

trait Interruptable[T <: WrapsUnit] extends UnitWithJob[T] {
  def interruptableNow = unit match {
    case gu: GroundUnit => gu.onGround
    case _              => true
  }
}
