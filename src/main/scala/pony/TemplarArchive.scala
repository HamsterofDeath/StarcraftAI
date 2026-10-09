package pony

import bwapi.{Unit => APIUnit, _}

class TemplarArchive(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
