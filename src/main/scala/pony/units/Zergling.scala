package pony
package units

import pony.combat.{ArmedMobile, FastAttackGround, GroundWeapon, MeleeWeapon, NormalGroundDamage}

import bwapi.{Unit => APIUnit, _}

class Zergling(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with NormalGroundDamage with Virtual with IsSmall
    with ArmedMobile with MeleeWeapon with FastAttackGround with ZergMobileUnit with CanBurrow
