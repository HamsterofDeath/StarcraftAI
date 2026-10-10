package pony
package units

import bwapi.{Unit => APIUnit, _}

class Nexus(unit: APIUnit) extends AnyUnit(unit) with MainBuilding
