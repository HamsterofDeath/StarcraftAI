package pony

import bwapi.{Unit => APIUnit, _}

class Citadel(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
