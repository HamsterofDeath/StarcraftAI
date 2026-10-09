package pony

import bwapi.{Unit => APIUnit, _}

class Overlord(unit: APIUnit)
    extends AnyUnit(unit) with MobileSupplyProvider with TransporterUnit with CanDetectHidden with IsBig
