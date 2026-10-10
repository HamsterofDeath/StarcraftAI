package pony
package units

import bwapi.{Unit => APIUnit, _}

class HydraliskDen(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
