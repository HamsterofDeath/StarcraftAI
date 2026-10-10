package pony
package units

import pony.combat.{AirWeapon, ArmedBuildingCoveringAir, ExplosiveAirDamage, SlowAttackAir}

import bwapi.{Unit => APIUnit, _}

class MissileTurret(unit: APIUnit)
    extends AnyUnit(unit) with ArmedBuildingCoveringAir with DetectorBuilding with SlowAttackAir with AirWeapon
    with ExplosiveAirDamage with TerranBuilding
