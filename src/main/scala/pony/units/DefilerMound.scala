package pony
package units

import bwapi.{Unit => APIUnit, _}

class DefilerMound(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
