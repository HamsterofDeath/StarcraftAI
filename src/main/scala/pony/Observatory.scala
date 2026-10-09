package pony

import bwapi.{Unit => APIUnit, _}

class Observatory(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
