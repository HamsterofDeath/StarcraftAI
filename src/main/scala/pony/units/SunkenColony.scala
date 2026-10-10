package pony
package units

import pony.combat.{ArmedBuildingCoveringGround, MediumAttackGround, NormalGroundDamage}

import bwapi.{Unit => APIUnit, _}

class SunkenColony(unit: APIUnit)
    extends AnyUnit(unit) with ZergBuilding with NormalGroundDamage with MediumAttackGround
    with ArmedBuildingCoveringGround
