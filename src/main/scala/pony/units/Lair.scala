package pony
package units

import bwapi.{Unit => APIUnit, _}

class Lair(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
