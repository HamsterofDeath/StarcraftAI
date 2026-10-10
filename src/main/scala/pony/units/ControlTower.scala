package pony
package units

import bwapi.{Unit => APIUnit, _}

class ControlTower(unit: APIUnit) extends AnyUnit(unit) with Upgrader with Addon with TerranBuilding
