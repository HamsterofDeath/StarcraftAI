package pony
package units

import pony.combat.{ArmedBuildingCoveringAir, MediumAttackAir, NormalAirDamage}

import bwapi.{Unit => APIUnit, _}

class SporeColony(unit: APIUnit)
    extends AnyUnit(unit) with DetectorBuilding with NormalAirDamage with MediumAttackAir with ZergBuilding
    with ArmedBuildingCoveringAir
