package pony

import bwapi.{Unit => APIUnit, _}

class Hydralisk(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundAndAirWeapon with ZergMobileUnit with ExplosiveAirDamage
    with ArmedMobile with ExplosiveGroundDamage with IsMedium with FastAttackAir with FastAttackGround with CanBurrow
    with Virtual
