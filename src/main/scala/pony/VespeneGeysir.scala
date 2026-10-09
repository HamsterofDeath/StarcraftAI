package pony

import bwapi.{Unit => APIUnit, _}

class VespeneGeysir(unit: APIUnit) extends AnyUnit(unit) with Geysir with Resource {
  myTilePosition.lockValueForever()
}
