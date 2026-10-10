package pony
package units

import bwapi.{Unit => APIUnit, _}

class Spire(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
