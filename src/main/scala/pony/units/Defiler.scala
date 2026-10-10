package pony
package units

import bwapi.{Unit => APIUnit, _}

class Defiler(unit: APIUnit) extends AnyUnit(unit) with ZergMobileUnit with GroundUnit with IsMedium
