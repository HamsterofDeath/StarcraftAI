package pony

import bwapi.{Unit => APIUnit, _}

class ScienceFacility(unit: APIUnit)
    extends AnyUnit(unit) with Upgrader with CanBuildAddons with UpgradeLimitLifter with TerranBuilding
