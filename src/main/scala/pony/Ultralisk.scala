package pony

import bwapi.{Unit => APIUnit, _}

class Ultralisk(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with IsBig with GroundWeapon with MeleeWeapon with GroundUnit
    with NormalGroundDamage with ArmedMobile with FastAttackGround
