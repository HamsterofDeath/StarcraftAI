package pony
package units

import bwapi.{Unit => APIUnit, _}

class GreaterSpire(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
