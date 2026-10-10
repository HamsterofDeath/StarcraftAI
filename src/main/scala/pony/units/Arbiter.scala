package pony
package units

import pony.combat.{
  ArmedMobile, ExplosiveAirDamage, ExplosiveGroundDamage, GroundAndAirWeapon, MediumAttackAir, MediumAttackGround
}

import bwapi.{Unit => APIUnit, _}

class Arbiter(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsBig with IsShip with ArmedMobile
    with ExplosiveAirDamage with ExplosiveGroundDamage with MediumAttackAir with MediumAttackGround
