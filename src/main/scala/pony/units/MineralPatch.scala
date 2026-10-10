package pony
package units

import bwapi.{Unit => APIUnit, _}

class MineralPatch(unit: APIUnit) extends AnyUnit(unit) with Resource {
  def isBeingMined = nativeUnit.isBeingGathered

  def hasRemainingMinerals = remainingMinerals > 8

  myTilePosition.lockValueForever()

  def remainingMinerals = remaining
}
