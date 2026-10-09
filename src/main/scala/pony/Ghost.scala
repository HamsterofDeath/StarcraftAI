package pony

import bwapi.{Unit => APIUnit, _}

class Ghost(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with CanCloak with FastAttackAir with ArmedMobile
    with VeryFastAttackGround with HasSingleTargetSpells with MobileRangeWeapon with IsSmall with IsInfantry
    with ConcussiveAirDamage with ConcussiveGroundDamage with VirtualCloakHelpers {
  override type CasterType = Ghost
  override val spells = List(Spells.Lockdown)
}
