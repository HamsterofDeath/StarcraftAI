package pony
package units

import bwapi.{Unit => APIUnit, _}

class Shuttle(unit: APIUnit) extends AnyUnit(unit) with TransporterUnit with SupportUnit with IsBig
