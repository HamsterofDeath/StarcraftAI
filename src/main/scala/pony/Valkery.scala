package pony

import bwapi.{Unit => APIUnit, _}

class Valkery(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with MobileRangeWeapon with IsBig with IsShip
    with ArmedMobile with BadDancer with ExplosiveAirDamage with SlowAttackAir
