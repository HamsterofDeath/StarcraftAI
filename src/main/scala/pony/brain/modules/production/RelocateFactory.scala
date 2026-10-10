package pony
package brain
package modules
package production

import pony.brain.jobs.{Employer, UnitWithJob}
import pony.brain.modules.wall.DepotRelocation
import pony.geometry.MapTilePosition
import pony.terrain.{ConstructionSiteFinder, ResourceArea}
import pony.units.{Factory, Mobile}

/** Lift one factory, fly it to the open field nearest home and land it there. */
private[pony] class RelocateFactory(
    employer: Employer[Factory],
    factory: Factory,
    claimedFields: collection.mutable.Set[Int]
) extends UnitWithJob[Factory](employer, factory, Priority.Expand) {
  private var destination      = Option.empty[MapTilePosition]
  private var destinationField = Option.empty[ResourceArea]
  private var phase            = Option.empty[DepotRelocation.Step]
  private var lastTile         = factory.tilePosition
  private var lastProgress     = currentTick
  private var landingRetries   = 0
  private var failedFields     = Set.empty[Int]
  private var lastTelemetry    = 0

  private def home = bases.mainBase.map(_.mainBuilding.tilePosition)

  private def safe(area: ResourceArea) =
    mapLayers.slightlyDangerousAsBlocked.free(area.nearbyFreeTile) &&
      unitGrid.enemy.allInRange[Mobile](area.nearbyFreeTile, 12).isEmpty

  private def chooseDestination(): Unit = {
    val homeTile   = home.getOrElse(factory.tilePosition)
    val homeAreaId = bases.mainBase.flatMap(_.resourceArea).map(_.uniqueId)
    val forceHome  = factory.isFloating && failedFields.size >= 2
    val found      = if (forceHome) None
    else strategicMap.resources.filterNot(a => homeAreaId.contains(a.uniqueId))
      .filterNot(a => failedFields.contains(a.uniqueId) || claimedFields.contains(a.uniqueId))
      .filter(safe)
      .toVector.sortBy(a => (a.nearbyFreeTile.distanceSquaredTo(homeTile), a.uniqueId))
      .iterator.flatMap { area =>
        new ConstructionSiteFinder(universe).findSpotFor(area.nearbyFreeTile, classOf[Factory], maxRange = 35)
          .map(area -> _)
      }.take(1).toList.headOption
    destination = found.map(_._2)
    destinationField = found.map(_._1)
    found.foreach { case (area, _) => claimedFields += area.uniqueId }
    if (destination.isEmpty && factory.isFloating) {
      // Never strand a lifted factory: fall back to the home plateau.
      destination = new ConstructionSiteFinder(universe).findSpotFor(homeTile, classOf[Factory], maxRange = 25)
      destinationField = None
    }
  }

  override def everyNth                      = 31
  override def shortDebugString              = "Relocate factory: " + phase
  override def isFinished                    = phase.contains(DepotRelocation.Established)
  override def jobHasFailedWithoutDeath      = false
  override def ordersForTick: Seq[UnitOrder] = {
    if (destination.isEmpty) chooseDestination()
    destination.toList.flatMap { landingTile =>
      val tile = factory.tilePosition
      if (tile != lastTile) { lastTile = tile; lastProgress = currentTick }
      val landedThere = !factory.isFloating && factory.nativeUnit.isCompleted && tile == landingTile
      val next        = DepotRelocation.next(
        true,
        false,
        factory.isFloating,
        tile.distanceToIsLess(landingTile, 4),
        landedThere
      )
      if (!phase.contains(next)) NativeMatchEvidence.trace(
        "factory-flight",
        s"id=${factory.nativeUnitId} phase=$next from=$tile to=$landingTile field=${destinationField.map(_.uniqueId).getOrElse(-1)}"
      )
      phase = Some(next)
      next match {
        case DepotRelocation.Lift =>
          if (factory.nativeUnit.canLift()) Orders.LiftBuilding(factory).toSeq else Nil
        case DepotRelocation.Fly =>
          if (currentTick - lastTelemetry > 240) {
            lastTelemetry = currentTick
            NativeMatchEvidence.trace(
              "factory-flight",
              s"id=${factory.nativeUnitId} phase=Fly at=${factory.tilePosition} moving=${factory.nativeUnit.isMoving} to=$landingTile sinceProgress=${currentTick -
                  lastProgress}"
            )
          }
          val stuck = currentTick - lastProgress > 24 * 120
          if (stuck && factory.isFloating && factory.nativeUnit.canLand(factory.tilePosition.asTilePosition)) {
            // Cannot reach the field; settle where we are instead of hovering forever.
            destination = Some(factory.tilePosition)
            destinationField = None
            lastProgress = currentTick
            Nil
          } else if (destinationField.exists(f => !safe(f)) || stuck) {
            destinationField.foreach(f => failedFields += f.uniqueId)
            destination = None
            destinationField = None
            lastProgress = currentTick
            Nil
          } else if (!factory.isFloating) {
            // Landed somewhere unexpected mid-flight; lift again and keep going.
            if (factory.nativeUnit.canLift()) Orders.LiftBuilding(factory).toSeq else Nil
          } else if (factory.nativeUnit.isMoving) Nil
          else {
            // Long flights can stall at the edge of what the engine can path for a building;
            // hop toward the target so every order makes bounded progress.
            val here     = factory.tilePosition
            val distance = math.max(math.abs(landingTile.x - here.x), math.abs(landingTile.y - here.y))
            val step     = math.min(8, distance)
            val towards  = MapTilePosition(
              here.x + Integer.signum(landingTile.x - here.x) * step,
              here.y + Integer.signum(landingTile.y - here.y) * step
            )
            Orders.FlyBuilding(factory, towards).toSeq
          }
        case DepotRelocation.Land =>
          if (factory.nativeUnit.canLand(landingTile.asTilePosition)) {
            landingRetries = 0
            Orders.LandBuilding(factory, landingTile).toSeq
          } else {
            landingRetries += 1
            if (landingRetries >= 8) {
              // Units can temporarily occupy the footprint. Search nearby instead of stranding a lifted factory.
              val alternatives = mapLayers.rawWalkableMap.spiralAround(landingTile, 8)
              alternatives.find(p => factory.nativeUnit.canLand(p.asTilePosition)).foreach { alternate =>
                destination = Some(alternate)
              }
              if (landingRetries >= 32) { destination = None; landingRetries = 0 }
            }
            Nil
          }
        case _ => Nil
      }
    }
  }
}
