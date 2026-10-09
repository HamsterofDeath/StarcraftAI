package pony

import bwapi.{Unit => APIUnit, _}

class Devourer(unit: APIUnit) extends AnyUnit(unit) with ZergMobileUnit with GroundUnit with IsBig
