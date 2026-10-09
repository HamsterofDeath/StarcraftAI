package pony

import bwapi.{Unit => APIUnit, _}

class SporeColony(unit: APIUnit)
    extends AnyUnit(unit) with DetectorBuilding with NormalAirDamage with MediumAttackAir with ZergBuilding
    with ArmedBuildingCoveringAir
