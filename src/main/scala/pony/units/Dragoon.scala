package pony
package units

import pony.combat.{
  ArmedMobile, ExplosiveAirDamage, ExplosiveGroundDamage, GroundAndAirWeapon, MediumAttackAir, MediumAttackGround
}

import bwapi.{Unit => APIUnit, _}

class Dragoon(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with Mechanic with IsBig with IsVehicle
    with ArmedMobile with ExplosiveGroundDamage with ExplosiveAirDamage
    with MediumAttackAir with MediumAttackGround
