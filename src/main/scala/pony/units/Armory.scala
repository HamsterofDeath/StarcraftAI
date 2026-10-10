package pony
package units

import bwapi.{Unit => APIUnit, _}

class Armory(unit: APIUnit) extends AnyUnit(unit) with Upgrader with TerranBuilding
