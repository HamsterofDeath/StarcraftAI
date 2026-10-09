package pony

import scala.jdk.CollectionConverters._

trait TransporterUnit extends AirUnit {
  override def isNonFighter = true
  private val myPickingUp   = oncePerTick {
    nativeUnit.getOrderTarget != null
  }
  private val myLoaded = oncePerTick {
    nativeUnit.getLoadedUnits.asScala.flatMap { u =>
      ownUnits.byNative(u).asInstanceOf[Option[GroundUnit]]
    }.toSet
  }

  def nearestDropTile = {
    ferryManager.nearestDropPointTo(currentTile).orElse {
      // over cliffs or water no drop point is known: take the one of the nearest walkable tile
      mapLayers.rawWalkableMap.nearestFree(currentTile).flatMap(ferryManager.nearestDropPointTo)
    }
  }

  def isPickingUp                = myPickingUp.get
  def loaded                     = myLoaded.get
  def isCarrying(gu: GroundUnit) = myLoaded(gu)
  def canDropHere                = ferryManager.canDropHere(currentTile)

  def hasUnitsLoaded = myLoaded.nonEmpty
}
