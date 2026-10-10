package pony
package units

import pony.combat.{ArmedMobile, FastAttackGround, GroundWeapon, MeleeWeapon, NormalGroundDamage}

import bwapi.{Unit => APIUnit, _}

class Ultralisk(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with IsBig with GroundWeapon with MeleeWeapon with GroundUnit
    with NormalGroundDamage with ArmedMobile with FastAttackGround
