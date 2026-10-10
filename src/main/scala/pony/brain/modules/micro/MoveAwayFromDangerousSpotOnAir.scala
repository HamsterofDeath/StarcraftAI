package pony
package brain
package modules
package micro

import pony.units.AirUnit

import bwapi.Color

import scala.reflect.ClassTag

class MoveAwayFromDangerousSpotOnAir(universe: Universe)
    extends AvoidSpecificAreas[AirUnit](universe) with DefaultDangerAreaConfig[AirUnit] {

  override protected def tilesToAvoidAsBlocked = {
    super.tilesToAvoidAsBlocked.map { base =>
      base.or(mapLayers.avoidanceSuggestionAir)
    }
  }

  override protected def debugColor = Color.Orange
}
