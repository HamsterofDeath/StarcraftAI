package pony
package units

import pony.combat.{
  ArmedMobile, GroundAndAirWeapon, HasSingleTargetSpells, MobileRangeWeapon, NormalAirDamage, NormalGroundDamage,
  VeryFastAttackAir, VeryFastAttackGround
}

import bwapi.{Unit => APIUnit, _}

class Battlecruiser(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with VeryFastAttackAir with VeryFastAttackGround
    with Mechanic with ArmedMobile with HasSingleTargetSpells with MobileRangeWeapon with IsBig with IsShip
    with NormalAirDamage with BadDancer with NormalGroundDamage {
  override val spells = Nil
}
