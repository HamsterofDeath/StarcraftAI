package pony

import bwapi.{Unit => APIUnit, _}

class Bunker(unit: APIUnit)
    extends AnyUnit(unit) with TerranBuilding with ArmedBuildingCoveringGroundAndAir
