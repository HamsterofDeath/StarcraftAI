package pony
package units

import bwapi.{Unit => APIUnit, _}

class Gateway(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
