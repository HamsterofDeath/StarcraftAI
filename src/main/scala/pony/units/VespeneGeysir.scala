package pony
package units

import bwapi.{Unit => APIUnit, _}

class VespeneGeysir(unit: APIUnit) extends AnyUnit(unit) with Geysir with Resource {
  myTilePosition.lockValueForever()
}
