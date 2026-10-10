package pony
package units

import bwapi.{Unit => APIUnit, _}

class Hive(unit: APIUnit) extends AnyUnit(unit) with MainBuilding
