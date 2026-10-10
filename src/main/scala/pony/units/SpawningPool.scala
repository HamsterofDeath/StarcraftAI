package pony
package units

import bwapi.{Unit => APIUnit, _}

class SpawningPool(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
