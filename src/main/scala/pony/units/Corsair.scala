package pony
package units

import pony.combat.{AirWeapon, ArmedMobile, ExplosiveAirDamage, FastAttackAir}

import bwapi.{Unit => APIUnit, _}

class Corsair(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with AirWeapon with Mechanic with IsMedium with IsShip with ExplosiveAirDamage
    with ArmedMobile with FastAttackAir
