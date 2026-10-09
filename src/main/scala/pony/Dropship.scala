package pony

import bwapi.{Unit => APIUnit, _}

class Dropship(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit with IsBig
