package pony
package brain
package modules

import bwapi.Color

import scala.reflect.ClassTag

class MoveAwayFromConstructionSite(universe: Universe)
    extends AvoidSpecificAreas[Mobile](universe) {
  override protected def tilesToAvoidAsBlocked = mapLayers.blockedByPlannedBuildings.toSome

  override protected val actionName = "<>"

  override protected def debugColor = Color.White
}
