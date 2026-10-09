package pony

import bwapi.{Unit => APIUnit, _}

class Barracks(unit: APIUnit) extends AnyUnit(unit) with UnitFactory with TerranBuilding
