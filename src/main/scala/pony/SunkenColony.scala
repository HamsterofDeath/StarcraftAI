package pony

import bwapi.{Unit => APIUnit, _}

class SunkenColony(unit: APIUnit)
    extends AnyUnit(unit) with ZergBuilding with NormalGroundDamage with MediumAttackGround
    with ArmedBuildingCoveringGround
