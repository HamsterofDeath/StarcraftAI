package pony

import bwapi.{Unit => APIUnit, _}

class EngineeringBay(unit: APIUnit) extends AnyUnit(unit) with Upgrader with TerranBuilding
