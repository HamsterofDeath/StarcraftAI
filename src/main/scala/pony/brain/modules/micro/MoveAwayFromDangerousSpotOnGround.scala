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

  /**
    * The campaign army walks into the reach of cannons, reavers and sieged tanks instead of hovering at its edge, and
    * comes home when it is losing (RunTerranCampaign); only hidden enemies it cannot see are still avoided. In game 2
    * vs Protoss on b65df6a a maxed army stood for half an hour at the foot of the enemy ramp, stepping out of the
    * cannons' reach, until the match timed out.
    */
  override protected def exempt(unit: GroundUnit) =
    worldDominationPlan.attackOf(unit).exists(_.campaign) &&
      mapLayers.coveredByEnemyCloakedGroundAsBlocked.free(unit.currentTile)

}
