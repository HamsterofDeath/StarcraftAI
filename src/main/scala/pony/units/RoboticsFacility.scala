package pony
package units

import bwapi.{Unit => APIUnit, _}

class RoboticsFacility(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with Upgrader
