package pony

import bwapi.{Unit => APIUnit, _}

class SupplyDepot(unit: APIUnit)
    extends AnyUnit(unit) with ImmobileSupplyProvider with TerranBuilding
