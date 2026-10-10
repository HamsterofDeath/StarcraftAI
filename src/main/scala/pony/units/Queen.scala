package pony
package units

import bwapi.{Unit => APIUnit, _}

class Queen(unit: APIUnit) extends AnyUnit(unit) with ZergMobileUnit with AirUnit with IsMedium
