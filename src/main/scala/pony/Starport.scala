package pony

import bwapi.{Unit => APIUnit, _}

class Starport(unit: APIUnit)
    extends AnyUnit(unit) with UnitFactory with CanBuildAddons with TerranBuilding
