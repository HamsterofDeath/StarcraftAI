package pony
package units

import bwapi.{Unit => APIUnit, _}

class RoboticsSupportBay(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
