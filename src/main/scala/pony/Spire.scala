package pony

import bwapi.{Unit => APIUnit, _}

class Spire(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
