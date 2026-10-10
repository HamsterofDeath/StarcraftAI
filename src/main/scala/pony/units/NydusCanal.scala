package pony
package units

import bwapi.{Unit => APIUnit, _}

class NydusCanal(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
