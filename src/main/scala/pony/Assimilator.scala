package pony

import bwapi.{Unit => APIUnit, _}

class Assimilator(unit: APIUnit) extends AnyUnit(unit) with Building with GasProvider
