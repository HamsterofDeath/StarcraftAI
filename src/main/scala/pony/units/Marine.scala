package pony
package units

import pony.combat.{
  ArmedMobile, FastAttackAir, FastAttackGround, GroundAndAirWeapon, HasSingleTargetSpells, MobileRangeWeapon,
  NormalAirDamage, NormalGroundDamage, Spells
}

import bwapi.{Unit => APIUnit, _}

class Marine(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanUseStimpack with MobileRangeWeapon
    with ArmedMobile with IsSmall with IsInfantry with NormalAirDamage with NormalGroundDamage
    with HasSingleTargetSpells with FastAttackAir with FastAttackGround {
  override type CasterType = CanUseStimpack
  override val spells = List(Spells.Stimpack)
}
