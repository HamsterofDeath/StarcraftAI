package pony
package units

import pony.combat.{
  ArmedMobile, FastAttackAir, FastAttackGround, GroundAndAirWeapon, NormalAirDamage, NormalGroundDamage
}

import bwapi.{Unit => APIUnit, _}

class Mutalisk(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with AirUnit with GroundAndAirWeapon with NormalGroundDamage
    with ArmedMobile with NormalAirDamage with IsMedium with FastAttackAir with FastAttackGround
