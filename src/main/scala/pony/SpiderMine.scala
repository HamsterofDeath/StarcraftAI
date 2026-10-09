package pony

import bwapi.{Unit => APIUnit, _}

class SpiderMine(unit: APIUnit)
    extends AnyUnit(unit) with SimplePosition with GroundUnit with IsSmall with AutoPilot {
  override def canMove = false
}
