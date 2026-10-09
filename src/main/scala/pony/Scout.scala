package pony

import bwapi.{Unit => APIUnit, _}

class Scout(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with GroundAndAirWeapon with Mechanic with IsBig with ExplosiveAirDamage
    with ArmedMobile with NormalGroundDamage with MediumAttackAir with FastAttackGround
