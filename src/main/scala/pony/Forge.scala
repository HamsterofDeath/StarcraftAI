package pony

import bwapi.{Unit => APIUnit, _}

class Forge(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
