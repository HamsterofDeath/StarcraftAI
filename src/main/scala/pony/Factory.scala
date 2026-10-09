package pony

import bwapi.{Unit => APIUnit, _}

class Factory(unit: APIUnit)
    extends AnyUnit(unit) with UnitFactory with CanBuildAddons with TerranBuilding
