package pony

import bwapi.{Unit => APIUnit, _}

class Mutalisk(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with AirUnit with GroundAndAirWeapon with NormalGroundDamage
    with ArmedMobile with NormalAirDamage with IsMedium with FastAttackAir with FastAttackGround
