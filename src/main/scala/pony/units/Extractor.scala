package pony
package units

import bwapi.{Unit => APIUnit, _}

class Extractor(unit: APIUnit) extends AnyUnit(unit) with GasProvider with ZergBuilding
