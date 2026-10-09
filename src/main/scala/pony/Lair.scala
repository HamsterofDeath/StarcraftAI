package pony

import bwapi.{Unit => APIUnit, _}

class Lair(unit: APIUnit) extends AnyUnit(unit) with ZergBuilding
