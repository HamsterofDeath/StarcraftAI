package pony

import bwapi.{Unit => APIUnit, _}

class Corsair(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with IsMedium with IsShip with ExplosiveAirDamage
    with ArmedMobile with FastAttackAir
