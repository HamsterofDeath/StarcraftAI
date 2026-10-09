package pony

import bwapi.{Unit => APIUnit, _}

class Gateway(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
