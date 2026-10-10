package pony
package brain
package modules
package micro

import pony.brain.modules.campaign.{CarpetQuotas, CarpetSpread}
import pony.brain.modules.production.{AlternativeBuildingSpot, BuildingRequestHelper}
import pony.brain.modules.wall.WallWithDepots
import pony.geometry.{Area, MapTilePosition, Size}
import pony.units.{SupplyDepot, Tank, WorkerUnit}

import scala.collection.mutable

/** With plenty of minerals, seal each stationary carpet tank behind a ring of depots. */
class TankBoxes(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe)
    with BuildingRequestHelper {
  private val done      = mutable.Set.empty[Int]
  private var activeBox = Option.empty[(Int, Vector[MapTilePosition])]

  private def carpet = strategy.current.usesCarpet
  private def wall   = universe.pluginByType[WallWithDepots]
  private def spread = universe.pluginByType[CarpetSpread]

  private def coveredAnchors = {
    val existing = ownUnits.allByType[SupplyDepot].filter(_.isInGame).map(_.tilePosition).toSet
    val pending  =
      (unitManager.requestedConstructions[SupplyDepot].flatMap(_.customPosition.requestedPosition) ++
        unitManager.constructionsInProgress[SupplyDepot].map(_.buildWhere)).toSet
    existing ++ pending
  }

  /** The tiles a melee unit could use to attack the tank, relative to its 2x2 footprint. */
  private def ringOf(t: MapTilePosition): Vector[MapTilePosition] =
    (for (dx <- -1 to 2; dy <- -1 to 2 if dx < 0 || dx > 1 || dy < 0 || dy > 1)
      yield MapTilePosition(t.x + dx, t.y + dy)).toVector

  private def walkableFree(t: MapTilePosition) =
    mapLayers.rawWalkableMap.insideBounds(t) && mapLayers.rawWalkableMap.free(t) &&
      mapLayers.blockedByBuildingTiles.free(t) && mapLayers.blockedByPlannedBuildings.free(t) &&
      mapLayers.blockedByResources.free(t)

  private def overlapsTank(anchor: MapTilePosition, tank: MapTilePosition) =
    anchor.x <= tank.x + 1 && tank.x <= anchor.x + 1 && anchor.y <= tank.y + 1 && tank.y <= anchor.y + 1

  /** Depot anchors that block every walkable attack tile; terrain provides the rest. */
  private def boxPlan(tank: Tank): Option[Vector[MapTilePosition]] = {
    val t         = tank.currentTile
    val needCover = ringOf(t).filter(walkableFree).distinct
    if (needCover.isEmpty) None
    else {
      val anchors = (for (dx <- -2 to 2; dy <- -2 to 2) yield MapTilePosition(t.x + dx, t.y + dy)).toVector
        .filterNot(a => overlapsTank(a, t)).filter(wall.depotSpotFree)
      val footprint        = anchors.map(a => a -> Area(a, Size(2, 2)).tiles.toVector).toMap
      val covers           = anchors.filter(a => footprint(a).exists(needCover.contains))
      val coveredBy        = covers.map(a => a -> footprint(a).filter(needCover.contains).toSet).toMap
      val tileToCandidates = needCover.map(r => r -> covers.filter(a => coveredBy(a).contains(r))).toMap
      val maxDepots        = math.min(CarpetQuotas.tankBoxMaxDepots, 12)
      var budget           = 20000
      def solve(uncovered: Set[MapTilePosition], chosen: Vector[MapTilePosition]): Option[Vector[MapTilePosition]] = {
        if (uncovered.isEmpty) Some(chosen)
        else if (chosen.size >= maxDepots || budget <= 0) None
        else {
          budget -= 1
          val target = uncovered.minBy(r => (r.y, r.x))
          tileToCandidates.getOrElse(target, Vector.empty)
            .filterNot(a => chosen.exists(b => (a.x - b.x).abs < 2 && (a.y - b.y).abs < 2))
            .sortBy(a => (-coveredBy(a).count(uncovered.contains), a.y, a.x))
            .iterator.flatMap(a => solve(uncovered -- coveredBy(a), chosen :+ a).iterator)
            .nextOption()
        }
      }
      solve(needCover.toSet, Vector.empty)
    }
  }

  override def onTick_!(): Unit = {
    if (!CarpetQuotas.tankBoxesEnabled || !carpet || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    if (!(wall.complete || wall.refused || wall.gateOpen)) return
    activeBox match {
      case Some((tankId, anchors)) =>
        val missing = anchors.filterNot(coveredAnchors)
        if (missing.isEmpty) {
          NativeMatchEvidence.trace("tank-box-complete", s"tank=$tankId depots=${anchors.size}")
          done += tankId
          activeBox = None
        } else if (!ownUnits.allByType[Tank].exists(t => t.isInGame && t.nativeUnitId == tankId)) {
          activeBox = None
        } else {
          missing.find(wall.depotSpotFree).foreach { a =>
            requestBuilding(
              classOf[SupplyDepot],
              takeCareOfDependencies = false,
              customBuildingPosition = AlternativeBuildingSpot.fromValidatedPreset(a)(wall.depotSpotFree(a)),
              priority = Priority.Expand
            )
            NativeMatchEvidence.trace("tank-box-depot", s"tank=$tankId at=$a")
          }
        }
      case None =>
        if (universe.resources.unlockedResources.minerals < CarpetQuotas.tankBoxMinMinerals) return
        val candidate = ownUnits.allByType[Tank]
          .filter(t => t.isInGame && !t.isBeingCreated && !t.nativeUnit.isMoving && !done(t.nativeUnitId))
          .toVector.sortBy(_.nativeUnitId)
          .iterator.flatMap { tank =>
            spread.postOf(tank.nativeUnitId)
              .filter(p => tank.currentTile.distanceToIsLess(p, 8))
              .flatMap(_ => boxPlan(tank).map(anchors => tank.nativeUnitId -> anchors))
          }.take(1).toList.headOption
        candidate.foreach { case (id, anchors) =>
          activeBox = Some((id, anchors))
          NativeMatchEvidence.trace("tank-box-plan", s"tank=$id depots=${anchors.size} at=${anchors.mkString(",")}")
        }
    }
  }
}
