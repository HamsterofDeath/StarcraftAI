package pony
package units

import pony.combat.{
  ArmedMobile, FastAttackAir, FastAttackGround, GroundAndAirWeapon, NormalAirDamage, NormalGroundDamage
}

import bwapi.{Unit => APIUnit, _}

class Interceptor(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsSmall with IsShip with ArmedMobile
    with NormalAirDamage with NormalGroundDamage with FastAttackAir with FastAttackGround
