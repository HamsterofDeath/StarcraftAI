package pony

import bwapi.{Unit => APIUnit, _}

class Zealot(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with IsSmall with IsInfantry with NormalGroundDamage
    with ArmedMobile with FastAttackGround
