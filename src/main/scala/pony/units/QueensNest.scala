package pony
package units

import bwapi.{Unit => APIUnit, _}

class QueensNest(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
