package pony

import bwapi.{Unit => APIUnit, _}

class Carrier(unit: APIUnit)
    extends AnyUnit(unit) with AirUnit with Mechanic with IsBig with ArmedMobile with IsShip
