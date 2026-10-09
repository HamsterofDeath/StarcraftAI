package pony

import bwapi.{Unit => APIUnit, _}

class Scarab(unit: APIUnit)
    extends AnyUnit(unit) with SimplePosition with Mobile with AutoPilot with IsSmall with GroundUnit
    with IndestructibleUnit
