package pony
package units

import bwapi.{Unit => APIUnit, _}

class InfestedCommandCenter(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
