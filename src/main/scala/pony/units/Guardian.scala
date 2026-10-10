package pony
package units

import pony.combat.{ArmedMobile, GroundWeapon, MediumAttackGround, NormalGroundDamage}

import bwapi.{Unit => APIUnit, _}

class Guardian(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with AirUnit with GroundWeapon with IsBig with NormalGroundDamage
    with ArmedMobile with MediumAttackGround
