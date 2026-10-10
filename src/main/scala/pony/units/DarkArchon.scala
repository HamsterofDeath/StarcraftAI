package pony
package units

import bwapi.{Unit => APIUnit, _}

class DarkArchon(unit: APIUnit) extends AnyUnit(unit) with GroundUnit with IsBig with IsInfantry
