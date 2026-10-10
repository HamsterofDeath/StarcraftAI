package pony
package brain
package modules
package production

import pony.brain.modules.micro.AvoidSpecificAreas
import pony.units.Mobile

import bwapi.Color

import scala.reflect.ClassTag

class MoveAwayFromConstructionSite(universe: Universe)
    extends AvoidSpecificAreas[Mobile](universe) {
  override protected def tilesToAvoidAsBlocked = mapLayers.blockedByPlannedBuildings.toSome

  override protected val actionName = "<>"

  override protected def debugColor = Color.White
}
