package pony
package units

import pony.combat.ArmedMobile

import bwapi.{Unit => APIUnit, _}

class Carrier(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with Mechanic with IsBig with ArmedMobile with IsShip
