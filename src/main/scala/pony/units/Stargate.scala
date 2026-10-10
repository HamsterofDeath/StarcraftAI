package pony
package units

import bwapi.{Unit => APIUnit, _}

class Stargate(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
