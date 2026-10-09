package pony

import pony.brain.Universe

class ConstructionSiteFinder(universe: Universe) {

  // initialisation happens in the main thread
  private val freeToBuildOn            = {
    universe.mapLayers.buildableBlockedByNothingTiles
    .mutableCopy
    .or_!(
      universe.mapLayers.blockedByPotentialAddons.mutableCopy)
    .or_!(
      universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
  }
  private val freeToBuildOnIgnoreUnits = {
    universe.mapLayers.freeTilesForConstruction.mutableCopy
    .or_!(universe.mapLayers.blockedByPotentialAddons
          .mutableCopy)
    .or_!(
      universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
    .guaranteeImmutability
  }

  private val outlineTouchCountArea = {
    universe.mapLayers.blockedByBuildingTiles.mutableCopy
    .or_!(universe.mapLayers.blockedByPotentialAddons.mutableCopy)
    .or_!(
      universe.mapLayers.blockedByPlannedBuildings.mutableCopy)
    .guaranteeImmutability
  }

  private val helper = new GeometryHelpers(universe.world.map.sizeX, universe.world.map.sizeY)
  private val resourceDepotBuffer = universe.mapLayers.blockedForResourceDeposit.mutableCopy.guaranteeImmutability

  /** Static construction masks preserve mineral traffic, depots and planned addons. */
  def bunkerSiteSafe(area: Area): Boolean = {
    BunkerSitePlacement.permitted(area, freeToBuildOnIgnoreUnits)
  }
  def bunkerSitesSafeTogether(areas: Seq[Area]): Boolean =
    BunkerSitePlacement.permittedTogether(areas, freeToBuildOnIgnoreUnits)
  def bunkerSites(resources: ResourceArea, workerCoverage: Seq[MapTilePosition] = Nil): Vector[Area] = {
    val tiles = resources.allPatchTiles.toVector ++ workerCoverage
    if (tiles.isEmpty) Vector.empty
    else (for {
      x <- (tiles.map(_.x).min - 6) to (tiles.map(_.x).max + 6)
      y <- (tiles.map(_.y).min - 6) to (tiles.map(_.y).max + 6)
      area = Area(MapTilePosition(x, y), Size(3, 2))
      if bunkerSiteSafe(area)
    } yield area).toVector
  }

  def forResourceArea(resources: ResourceArea): SubFinder = {
    val size = Size(4 + 2, 3) // include space for comsat
    //main thread
    val grid = freeToBuildOn.or_!(universe.mapLayers.blockedForResourceDeposit.mutableCopy)
    new SubFinder {
      override def find: Option[MapTilePosition] = {
        // background
        val possible = {
          helper.iterateBlockSpiralClockWise(resources.center, 35)
          .filter { candidate =>
            def correctArea = {
              val area = Area(candidate, size)
              grid.inBounds(area) && grid.free(area)
            }
            def lineOfSight = {
              def fromPatch = resources.allPatchTiles.exists { p =>
                AreaHelper.directLineOfSight(p, candidate,
                  universe.mapLayers.rawWalkableMap)
              }
              def fromGeysir = resources.allGeysirTiles.exists { p =>
                AreaHelper.directLineOfSight(p, candidate,
                  universe.mapLayers.rawWalkableMap)
              }
              fromPatch || fromGeysir
            }
            correctArea && lineOfSight
          }
          .toVector
        }

        if (possible.isEmpty) {
          None
        } else {
          val closest = possible.minBy { elem =>
            val area = Area(elem, size)
            val distanceToPatches = resources.allPatchTiles.map(e => area.distanceTo(e)).sum
            val distanceToGeysirs = resources.allGeysirTiles.map(e => area.distanceTo(e)).sum
            distanceToPatches + distanceToGeysirs
          }
          Some(closest)
        }
      }
    }
  }

  def findSpotFor[T <: Building](near: MapTilePosition, building: Class[? <: T], maxRange: Int = 75,
                                 bestOfN: Int = 256,
                                 preferNear: Option[MapTilePosition] = None,
                                 acceptableArea: Area => Boolean = _ => true) = {
    // this happens in the background
    val unitType = building.toUnitType
    val necessarySize = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    val addonSize = Size(2, 2)
    val necessarySizeAddon = if (unitType.canBuildAddon) {
      Some(addonSize)
    } else None

    val withStreets = freeToBuildOn.mutableCopy

    helper.iterateBlockSpiralClockWise(near, maxRange).flatMap { upperLeft =>
      val area = Area(upperLeft, necessarySize)
      val addonArea = necessarySizeAddon.map(Area(area.lowerRight.movedBy(1, -1), _))
      def containsArea = freeToBuildOn.inBounds(area) &&
                         addonArea.map(freeToBuildOn.inBounds).getOrElse(true)
      def free = {
        val checkIfBlocksSelf = freeToBuildOnIgnoreUnits.mutableCopy
        checkIfBlocksSelf.block_!(area)
        addonArea.foreach(checkIfBlocksSelf.block_!)
        def areaFree = withStreets.free(area) &&
                       addonArea.map(withStreets.free).getOrElse(true)
        def outlineFree = area.growBy(1).outline.forall {freeToBuildOnIgnoreUnits.freeAndInBounds}
        def noLock = checkIfBlocksSelf.areaCountExpensive == freeToBuildOnIgnoreUnits.areaCount

        areaFree && (outlineFree || noLock)
      }
      // Native depots cannot be placed inside the mineral/geyser exclusion zone, even at home.
      // A preferred anchor also has to stay reachable on foot from the base's own area.
      val reachable = preferNear.isEmpty ||
        universe.mapLayers.rawWalkableMap.areInSameWalkableArea(near, upperLeft)
      if (containsArea && reachable && acceptableArea(area) && ResourceDepotPlacement.permitted(area, unitType.isResourceDepot, resourceDepotBuffer) && free) {
        val freeSurroundingTiles = area.growBy(1).outline
                                   .count(outlineTouchCountArea.freeAndInBounds)
        val distanceToCenter = area.centerTile.distanceTo(near)
        Some((upperLeft, distanceToCenter / 6.0, freeSurroundingTiles))
      } else {
        None
      }
    }.take(bestOfN)
    .minByOpt { case (upperLeft, distanceKey, freeSurroundingTiles) =>
      (preferNear.map(p => upperLeft.distanceTo(p).toDouble / 6.0).getOrElse(distanceKey),
        freeSurroundingTiles)
    }
    .map(_._1)
  }
}
