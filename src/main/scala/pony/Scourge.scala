package pony

import bwapi.{Unit => APIUnit, _}

class Scourge(unit: APIUnit)
    extends AnyUnit(unit) with ZergMobileUnit with AirUnit with IsSmall with ArmedMobile
