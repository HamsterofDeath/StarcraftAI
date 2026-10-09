package pony

import bwapi.{Unit => APIUnit, _}

class Extractor(unit: APIUnit) extends AnyUnit(unit) with GasProvider with ZergBuilding
