package pony
package brain
package modules
package micro

import pony.units.GroundUnit

import bwapi.Color

import scala.reflect.ClassTag

class MoveAwayFromDangerousSpotOnGround(universe: Universe)
    extends AvoidSpecificAreas[GroundUnit](universe) with DefaultDangerAreaConfig[GroundUnit] {

  override protected def tilesToAvoidAsBlocked = {
    super.tilesToAvoidAsBlocked.map { base =>
      base.or(mapLayers.avoidanceSuggestionGround)
    }
  }

  override protected def debugColor = Color.Red

}
