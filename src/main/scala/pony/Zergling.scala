package pony

import bwapi.{Unit => APIUnit, _}

class Zergling(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with NormalGroundDamage with Virtual with IsSmall
    with ArmedMobile with MeleeWeapon with FastAttackGround with ZergMobileUnit with CanBurrow
