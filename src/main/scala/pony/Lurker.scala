package pony

import bwapi.{Unit => APIUnit, _}

class Lurker(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with GroundUnit with GroundWeapon with NormalGroundDamage with Virtual
    with IsBig with ArmedMobile with FastAttackGround with CanBurrow {}
