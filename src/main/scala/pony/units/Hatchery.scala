package pony
package units

import bwapi.{Unit => APIUnit, _}

class Hatchery(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
