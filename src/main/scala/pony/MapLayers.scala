package pony

import pony.brain.{HasUniverse, Universe}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

class MapLayers(override val universe: Universe) extends HasUniverse {
  private val myCurrentSafeInput = oncePerTick {
    new EvalSafeInput
  }

  type AreaFromCircle = FutureIterator[IterableOnce[Circle], Grid2D]
  private val rawMapWalk        = world.map.walkableGrid
  private val empty             = world.map.walkableGrid.emptySameSize(false)
                                  .guaranteeImmutability
  private val full              = empty.reverseView
  private val rawMapWalkMutable = world.map.walkableGrid.mutableCopy
  private val rawMapBuild       = world.map.buildableGrid.mutableCopy
  private val plannedBuildings  = world.map.empty.zoomedOut.mutableCopy

  private val mapKey = uniqueKey

  private def register[T <: Grid2D](gen: => T) = {
    val lazyVal = explicitly(mapKey, gen, synchronize = true)
    lazyVal.get
    lazyVal
  }

  private val justBuildings                   = register(evalOnlyBuildings)
  private val justMines                       = register(evalOnlyMines)
  private val justMineralsAndGas              = register(evalOnlyResources)
  private val justWorkerPaths                 = register(evalWorkerPaths)
  private val justBlockingMobiles             = register(evalOnlyMobileBlockingUnits)
  private val justAddonLocations              = register(evalPotentialAddonLocations)
  private val withBuildingsAndResources       = register(evalWithBuildingsAndResources)
  private val withEverythingStaticBuildable   = register(evalEverythingStaticBuildable)
  private val withEverythingStaticWalkable    = register(evalEverythingStaticWalkable)
  private val withEverythingBlockingBuildable = register(evalEverythingBlockingBuildable)
  private val withEverythingBlockingWalkable  = register(evalEverythingBlockingWalkable)

  private val cpuHeavy = ArrayBuffer.empty[FutureIterator[?, Grid2D]]

  implicit class RichFuture(val f: FutureIterator[?, Grid2D]) {
    def registeredAs(name: String) = {
      cpuHeavy += f.named(name)
      f.setupRecalcHint(Primes.prime43)
    }
  }

  // too cpu heavy to be done in the main thread
  private val justAreasToDefend                    = evalOnlyAreasToDefend
                                                     .registeredAs("Areas to defend")
  private val coveredByPsiStorm                    = evalUnderPsiStorm
                                                     .registeredAs("Unser psi storm")
                                                     .setupRecalcHint(Primes.prime2)
  private val justBlockedForMainBuilding           = evalOnlyBlockedForMainBuildings
                                                     .registeredAs("Main building blocked")
  private val coveredByDangerousUnits              = evalDangerousOnGround.registeredAs("Dangerous")
  private val coveredByHostileLongRangeGroundUnits = evalHostileLongRangeGroundUnits
                                                     .registeredAs("Long range ground hostiles")
  private val coveredByHostileCloakedGroundUnits   = evalHostileCloakedGroundUnits
                                                     .registeredAs("Cloaked ground hostiles")
                                                     .setupRecalcHint(Primes.prime11)
  private val coveredByHostileCloakedAirUnits      = evalHostileCloakedAirUnits
                                                     .registeredAs("Cloaked air hostiles")
                                                     .setupRecalcHint(Primes.prime11)
  private val coveredByHostileLongRangeAirUnits    = evalHostileLongRangeAirUnits
                                                     .registeredAs("Long range air hostiles")
  private val coveredByAnythingWithGroundWeapons   = evalSlightlyDangerousForGroundUnits
                                                     .registeredAs("Ground hostiles")
  private val coveredByAnythingWithAirWeapons = evalSlightlyDangerousForAirUnits
                                                .registeredAs("Air hostiles")
  private val coveredByOwnDetectors           = evalDetected.registeredAs("detected (self)")
  private val coveredByOwnGround              = evalGroundDefended
                                                .registeredAs("Covered by ground (self)")
  private val coveredbyOwnAir                 = evalAirDefended
                                                .registeredAs("Covered by air (self)")
  private val exposedToCloaked                = evalExposedToCloakedUnits
                                                .registeredAs("Exposed to cloaked units (self)")
  private val justBlockingMobilesExtended     = evalOnlyMobileBlockingUnitsExtended
                                                .registeredAs("Ground mobiles (self)")

  // initializion order mess
  private val walkableSafe                           = register(evalWalkableSafe)
  private val airSafe                                = register(evalAirSafe)
  private val withEverythingSlightlyDangerousBlocked = register(evalSlightlyDangerousForAnyUnit)
  private val withAvoidSuggestionGroundBlocked       = register(evalAvoidanceSuggestionGround)
  private val withAvoidSuggestionAirBlocked          = register(evalAvoidanceSuggestionAir)

  private var lastUpdatePerformedInTick = universe.currentTick

  def isOnIsland(tilePosition: MapTilePosition) = {
    val areaInQuestion = rawMapWalk.areaOf(tilePosition)
    val maxArea = rawWalkableMap.areas.sortBy(-_.freeCount)
    if (maxArea.size > 1) {
      val main = maxArea.head
      val second = maxArea(1)
      val assumeIslandMap = second.freeCount * 2 > main.freeCount
      assumeIslandMap || !areaInQuestion.contains(main)
    } else false

  }

  def rawWalkableMap = rawMapWalk

  def slightlyDangerousAsBlocked = withEverythingSlightlyDangerousBlocked.get

  def avoidanceSuggestionGround = withAvoidSuggestionGroundBlocked.get

  def avoidanceSuggestionAir = withAvoidSuggestionAirBlocked.get

  def defendedTiles = {
    justAreasToDefend.getOrElse(empty)
  }

  def exposedToCloakedUnits = {
    exposedToCloaked.getOrElse(full)
  }

  def blockedByPotentialAddons = {
    justAddonLocations.asReadOnlyView
  }

  def underPsiStorm = coveredByPsiStorm.getOrElse(emptyGrid)

  def blockedByPlannedBuildings = plannedBuildings.asReadOnlyView

  def coveredByDetectors = coveredByOwnDetectors.getOrElse(emptyGrid)

  def coveredByOwnGroundUnits = coveredByOwnGround.getOrElse(emptyGrid)

  def coveredByOwnAirUnits = coveredbyOwnAir.getOrElse(emptyGrid)

  def dangerousAsBlocked = coveredByDangerousUnits.getOrElse(emptyGrid)

  def coveredByEnemyLongRangeGroundAsBlocked = coveredByHostileLongRangeGroundUnits
                                               .getOrElse(emptyGrid)

  def coveredByEnemyCloakedGroundAsBlocked = coveredByHostileCloakedGroundUnits
                                             .getOrElse(emptyGrid)

  def coveredByEnemyCloakedAirAsBlocked = coveredByHostileCloakedAirUnits
                                          .getOrElse(emptyGrid)

  def coveredByEnemyLongRangeAirAsBlocked = coveredByHostileLongRangeAirUnits
                                            .getOrElse(emptyGrid)

  def slightlyDangerousForGroundAsBlocked = coveredByAnythingWithGroundWeapons
                                            .getOrElse(emptyGrid)

  def slightlyDangerousForAirAsBlocked = coveredByAnythingWithAirWeapons
                                         .getOrElse(emptyGrid)

  def freeTilesForConstruction = {
    withEverythingStaticBuildable.asReadOnlyView
  }

  def buildableBlockedByNothingTiles = {
    withEverythingBlockingBuildable.asReadOnlyView
  }

  def freeWalkableTiles = {
    withEverythingBlockingWalkable.asReadOnlyView
  }

  def freeWalkableIgnoringMobiles = {
    withEverythingStaticWalkable.asReadOnlyView
  }

  def blockedByBuildingTiles = {
    justBuildings.asReadOnlyView
  }

  def blockedByResources = {
    justMineralsAndGas.asReadOnlyView
  }

  def blockedByMines = {
    justMines.asReadOnlyView
  }

  def blockedForResourceDeposit = {
    justBlockedForMainBuilding.getOrElse(empty)
  }

  def blockedByWorkerPaths = {
    justWorkerPaths.asReadOnlyView
  }

  def blockedByMobileUnits = {
    justBlockingMobiles.asReadOnlyView
  }

  def blockedByMobileUnitsExtended = {
    justBlockingMobilesExtended.getOrElse(empty)
  }

  def blockBuilding_!(where: Area): Unit = {
    plannedBuildings.block_!(where)
  }

  def unblockBuilding_!(where: Area): Unit = {
    plannedBuildings.free_!(where)
  }

  def tick(): Unit = {
    super.onTick_!()
    update()
  }

  def emptyGrid = world.map.emptyZoomed

  def safeGround = walkableSafe

  def safeAir = airSafe

  private def evalWorkerPaths = {
    trace("Re-evaluation of worker paths")
    val ret = emptyCopy
    bases.allBases.foreach { base =>
      base.myMineralGroup.foreach { group =>
        group.patches.foreach { patch =>
          base.mainBuilding.area.outline.foreach { outline =>
            patch.area.tiles.foreach { patchTile =>
              ret.block_!(outline, patchTile)
            }
          }
        }
      }
      base.myGeysirs.foreach { geysir =>
        base.mainBuilding.area.outline.foreach { tile1 =>
          geysir.area.outline.foreach { tile2 =>
            ret.block_!(tile1, tile2)
          }
        }
      }
    }
    ret
  }

  private def update(): Unit = {
    if (lastUpdatePerformedInTick != universe.currentTick) {
      lastUpdatePerformedInTick = universe.currentTick

      invalidate(mapKey)

      cpuHeavy.iterator
      .filter(_.triggerRecalcOn(currentTick))
      .foreach(_.prepareNextIfDone())
    }
  }

  private def evalWithBuildingsAndResources = justBuildings.mutableCopy.or_!(justMineralsAndGas)

  private def evalOnlyBuildings = evalOnlyUnits(ownUnits.allByType[Building].filterNot(_.isFloating))

  private def evalOnlyAreasToDefend = {
    evalOnlyUnitsAsync(ownUnits.allByType[Building].filterNot(b => b.isFloating || b.isInstanceOf[DetectorBuilding]), 8)
  }

  private def evalOnlyUnitsAsync(units: => IterableOnce[StaticallyPositioned], growBy: Int) = {
    def areas = units.iterator.map(_.area)
    FutureIterator.feed(areas.toVector).produceAsync { unitAreas =>
      val ret = emptyCopy
      unitAreas.foreach { a =>
        val by = a.growBy(growBy)
        ret.block_!(by)
      }
      ret.guaranteeImmutability
    }
  }

  private def emptyCopy = world.map.emptyZoomed.mutableCopy

  private def evalOnlyBlockedForMainBuildings = evalOnlyBlockedResourceAreas(
    ownUnits.allByType[Resource])

  private def evalOnlyBlockedResourceAreas(units: => IterableOnce[Resource]) = {
    def areas = units.iterator.map(_.blockingAreaForMainBuilding)
    FutureIterator.feed(areas).produceAsync { in =>
      val ret = emptyCopy
      in.iterator.foreach { area =>
        ret.block_!(area)
      }
      ret.guaranteeImmutability
    }
  }

  private def evalPotentialAddonLocations = evalOnlyAddonAreas(ownUnits.allByType[CanBuildAddons].filterNot(_.isFloating))

  private def evalOnlyAddonAreas(units: IterableOnce[CanBuildAddons]) = {
    val ret = emptyCopy
    units.iterator.foreach { b =>
      ret.block_!(b.addonArea)
    }
    ret
  }

  private def evalOnlyMobileBlockingUnits = evalOnlyMobileUnits(
    ownUnits.allByType[GroundUnit].iterator.filter(_.onGround))

  private def evalOnlyMobileBlockingUnitsExtended = FutureIterator.feed(blockedByMobileUnits)
                                                    .produceAsync { in =>
                                                      in.mutableCopy.addOutlineToBlockedTiles_!()
                                                      .guaranteeImmutability
                                                    }

  private def evalOnlyMines = evalOnlyMobileUnits(ownUnits.allByType[SpiderMine])

  private def evalOnlyMobileUnits(units: IterableOnce[GroundUnit]) = {
    val ret = emptyCopy
    units.iterator.foreach { b =>
      ret.block_!(b.currentTile)
    }
    ret
  }

  private def evalOnlyResources = evalOnlyUnits(
    ownUnits.allByType[MineralPatch].filter(_.remaining > 0))
                                  .or_!(evalOnlyUnits(ownUnits.allByType[Geysir]))

  private def evalOnlyUnits(units: IterableOnce[StaticallyPositioned]) = {
    val ret = emptyCopy
    units.iterator.foreach { b =>
      val by = b.area
      ret.block_!(by)
    }
    ret
  }

  private def evalEverythingStaticBuildable = withBuildingsAndResources.mutableCopy
                                              .or_!(plannedBuildings)
                                              .or_!(justWorkerPaths)
                                              .or_!(rawMapBuild)

  private def evalEverythingStaticWalkable = withBuildingsAndResources.mutableCopy
                                             .or_!(plannedBuildings)
                                             .or_!(rawMapWalkMutable)

  private def evalEverythingBlockingBuildable = withEverythingStaticBuildable.mutableCopy
                                                .or_!(justBlockingMobiles)

  private def evalSlightlyDangerousForAnyUnit = slightlyDangerousForGroundAsBlocked
                                                .or(slightlyDangerousForAirAsBlocked)

  private def evalAvoidanceSuggestionGround = coveredByEnemyLongRangeGroundAsBlocked
                                              .or(coveredByEnemyCloakedGroundAsBlocked)

  private def evalAvoidanceSuggestionAir = coveredByEnemyLongRangeAirAsBlocked
                                           .or(coveredByEnemyCloakedAirAsBlocked)

  private def evalEverythingBlockingWalkable = withEverythingStaticWalkable.mutableCopy
                                               .or_!(justBlockingMobiles)

  private def evalWalkableSafe = withEverythingStaticWalkable
                                 .or(dangerousAsBlocked)

  private def evalAirSafe = emptyCopy
                            .or_!(slightlyDangerousForAirAsBlocked.mutableCopy)
                            .asReadOnlyView

  private def evalDetected = areaOfCircles {
    universe.ownUnits.allDetectors.map(_.detectionArea)
  }

  private def areaOfCircles(trav: => IterableOnce[Circle]): AreaFromCircle = {
    areaOfCircles(block = true)(trav)
  }

  private def areaOfCircles(block: Boolean)(trav: => IterableOnce[Circle]): AreaFromCircle = {
    FutureIterator.feed(trav).produceAsync { in =>
      val base = if (block) emptyCopy.mutableCopy else emptyCopy.mutableCopy.invertedMutable
      for (circle <- in.iterator; tile <- circle.asTiles) {
        if (block) {
          base.block_!(tile)
        } else {
          base.free_!(tile)
        }
      }
      base.guaranteeImmutability
    }
  }

  private def evalExposedToCloakedUnits = areaOfCircles(block = false) {
    universe.ownUnits.allDetectors.map(_.detectionArea)
  }

  private def evalGroundDefended = areaOfCircles {
    universe.ownUnits.allWithGroundWeapon.map(_.inGroundWeaponRange)
  }

  private def evalAirDefended = areaOfCircles {
    universe.ownUnits.allWithAirWeapon.map(_.inAirWeaponRange)
  }

  private def evalDangerousOnGround = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.enemy.buildings, 12, 3).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.units, 12, 5).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.armedBuildingsGround, 12, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def safeInputForCurrentTick = {
    myCurrentSafeInput.get
  }

  private def evalHostileLongRangeGroundUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections
      .tilesInCircleWithRange(in.enemy.longRangeGroundCoveringUnits, 7, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalHostileCloakedGroundUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircleWithRange(in.enemy.cloakedGroundCoveringUnits, 4, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalUnderPsiStorm = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.own.ownUnitsUnderPsi, 2, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalHostileCloakedAirUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircleWithRange(in.enemy.cloakedAirCoveringUnits, 4, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalHostileLongRangeAirUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircleWithRange(in.enemy.longRangeAirCoveringUnits, 7, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalSlightlyDangerousForGroundUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.enemy.units, 12, 1).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.armedBuildingsGround, 12, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private def evalSlightlyDangerousForAirUnits = {
    FutureIterator.feed(safeInputForCurrentTick).produceAsync { in =>
      val base = in.base
      base.geoHelper.intersections.tilesInCircle(in.enemy.units, 12, 1).foreach(base.block_!)
      base.geoHelper.intersections.tilesInCircle(in.enemy.armedBuildingsAir, 12, 1)
      .foreach(base.block_!)
      base.guaranteeImmutability
    }
  }

  private class EvalSafeInput {
    private val baseTemplate = emptyCopy

    def base = baseTemplate.mutableCopy

    class EnemyData {
      val buildings            = universe.enemyUnits.allBuildings.map(_.centerTile)
      val armedBuildingsGround = universe.enemyUnits.allBuildingsWithGroundWeapons.map(_.centerTile)
      val armedBuildingsAir    = universe.enemyUnits.allBuildingsWithAirWeapons.map(_.centerTile)

      val units = {
        universe.enemyUnits.allMobiles.filterNot(_.isHarmlessNow).map(_.currentTile)
      }

      val longRangeGroundCoveringUnits = {
        universe.enemyUnits.allWithGroundWeapon.filterNot(_.isHarmlessNow)
        .filter(_.groundRangeTiles >= 7)
        .map { e =>
          e.centerTile -> e.groundRangeTiles
        }
      }
      val cloakedGroundCoveringUnits   = {
        universe.enemyUnits.allWithGroundWeapon.filterNot(_.isHarmlessNow)
        .collect { case cd: CanHide if cd.isHidden => cd }
        .map { e =>
          e.centerTile -> e.groundRangeTiles
        }
      }
      val cloakedAirCoveringUnits      = {
        universe.enemyUnits.allWithAirWeapon.filterNot(_.isHarmlessNow)
        .collect { case cd: CanHide if cd.isHidden => cd }
        .map { e =>
          e.centerTile -> e.airRangeTiles
        }
      }
      val longRangeAirCoveringUnits    = {
        universe.enemyUnits.allWithAirWeapon.filterNot(_.isHarmlessNow)
        .filter(_.airRangeTiles >= 7)
        .map { e =>
          e.centerTile -> e.airRangeTiles
        }
      }
    }

    class OwnData {
      val ownUnitsUnderPsi = {
        universe.ownUnits.allMobiles
        .filter(_.wasUnderPsiStormSince(48))
        .flatMap(_.lastKnownStormPosition)
      }
    }

    val enemy = new EnemyData
    val own = new OwnData
  }
}
