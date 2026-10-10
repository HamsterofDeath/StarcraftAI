package pony
package units

import pony.combat.{
  ArmedMobile, ExplosiveAirDamage, FastAttackGround, GroundAndAirWeapon, HasSingleTargetSpells, MediumAttackAir,
  MobileRangeWeapon, NormalGroundDamage, Spells
}

import bwapi.{Unit => APIUnit, _}

class Wraith(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with CanCloak with FastAttackGround with MediumAttackAir
    with Mechanic with MobileRangeWeapon with IsBig with IsShip with NormalGroundDamage with BadDancer
    with VirtualCloakHelpers with ExplosiveAirDamage with ArmedMobile with HasSingleTargetSpells {

  override type CasterType = Wraith
  override val spells = List(Spells.WraithCloak)
}
