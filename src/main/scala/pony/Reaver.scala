package pony

import bwapi.{Unit => APIUnit, _}

class Reaver(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with GroundWeapon with Mechanic with IsBig with IsVehicle with ArmedMobile
    with NormalGroundDamage with SlowAttackGround
