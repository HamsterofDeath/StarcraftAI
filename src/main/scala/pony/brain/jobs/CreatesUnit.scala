package pony
package brain
package jobs

import pony.units.WrapsUnit

trait CreatesUnit[T <: WrapsUnit] extends UnitWithJob[T]
