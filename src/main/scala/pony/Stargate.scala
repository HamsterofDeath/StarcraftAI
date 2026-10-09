package pony

import bwapi.{Unit => APIUnit, _}

class Stargate(unit: APIUnit) extends AnyUnit(unit) with UnitFactory
