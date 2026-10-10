package pony
package units

import pony.combat.{
  ArmedMobile, ExplosiveAirDamage, FastAttackGround, GroundAndAirWeapon, MobileRangeWeapon, NormalGroundDamage,
  SlowAttackAir
}

import bwapi.{Unit => APIUnit, _}

class Goliath(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with FastAttackGround with SlowAttackAir with Mechanic
    with MobileRangeWeapon with IsBig with IsVehicle with NormalGroundDamage with BadDancer with ExplosiveAirDamage
    with ArmedMobile
