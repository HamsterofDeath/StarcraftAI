package pony

import bwapi.{Unit => APIUnit, _}

class Broodling(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with NormalGroundDamage with IsSmall with ArmedMobile
    with FastAttackGround with ZergMobileUnit
