package pony

import bwapi.{Unit => APIUnit, _}

class FleetBeacon(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
