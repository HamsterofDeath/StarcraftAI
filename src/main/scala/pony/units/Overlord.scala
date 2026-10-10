package pony
package units

import bwapi.{Unit => APIUnit, _}

class Overlord(unit: APIUnit)
    extends AnyUnit(unit) with MobileSupplyProvider with TransporterUnit with CanDetectHidden with IsBig
