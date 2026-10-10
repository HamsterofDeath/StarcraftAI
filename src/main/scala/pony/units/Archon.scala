package pony
package units

import pony.combat.{
  ArmedMobile, FastAttackAir, FastAttackGround, GroundAndAirWeapon, NormalAirDamage, NormalGroundDamage
}

import bwapi.{Unit => APIUnit, _}

class Archon(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with IsBig with IsInfantry with NormalAirDamage
    with ArmedMobile with NormalGroundDamage with FastAttackAir with FastAttackGround
