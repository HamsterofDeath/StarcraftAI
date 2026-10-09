package pony
package brain
package modules

import scala.collection.mutable

class SetupAntiCloakDefenses(universe: Universe)
    extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {

  private val richBaseCount = oncePerTick {
    (bases.richBasesCount / 2) max 1
  }
  private val analyzed = FutureIterator.feed(input).produceAsyncLater { in =>
    val counts = mutable.HashMap.empty[MapTilePosition, Int]

    // goal: position detectors so that all values are >= 0
    in.exposed.allBlocked
      .filter(in.ownArea.blocked)
      .foreach { where =>
        val isIsland = mapLayers.isOnIsland(where)
        counts.put(where, -in.limit - (if (isIsland) 2 else 0))
      }

    in.existing.foreach { case (detector, circle) =>
      circle.asTiles.foreach { where =>
        if (counts.contains(where)) {
          counts.insertReplace(where, _ + 1 min 0, 0)
        }
      }
    }

    val exposure = -counts.values.sum

    if (exposure > 0) {
      // find the next spot that minimizes the exposure
      val byPriority = counts.keySet.toVector.sortBy { candidate =>
        val covered = {
          geoHelper.circle(candidate, 8).asTiles.count { tile =>
            counts.get(tile) match {
              case Some(value) if value < 0 => true
              case _                        => false
            }
          }
        }
        exposure - covered
      }
      // of those, take the first one that is ok
      val bestPosition = byPriority.iterator.flatMap { where =>
        in.constructionSiteFinder.findSpotFor(where, in.buildingType, 1, 1)
      }.nextOption()
      info(s"Next detector should be built at ${bestPosition.get}", bestPosition.isDefined)
      trace(s"Exposed by $exposure, but cannot add detector building", bestPosition.isEmpty)
      bestPosition
    } else {
      None
    }
  }.named("Anti cloak defendes")

  override def onTick_!() = {
    super.onTick_!()
    kickOffOn24thTick()
    val active = strategy.current.buildAntiCloakNow
    analyzed.foreach { bestBuildingLocation =>
      if (active) {
        val buildingInProgress = {
          unitManager.constructionsInProgress[DetectorBuilding].nonEmpty
        }
        def buildingExists = {
          bestBuildingLocation.exists { where =>
            ownUnits.buildingAt(where).isDefined
          }
        }
        def outdated = {
          analyzed.lastUsedFeed.map(_.age).getOrElse(0) > 24 * 15
        }

        if (buildingInProgress || outdated || buildingExists) {
          analyzed.prepareNextIfDone()
        } else {
          bestBuildingLocation.foreach { where =>
            requestBuilding(
              targetBuildingType,
              takeCareOfDependencies = true,
              customBuildingPosition = AlternativeBuildingSpot.fromPreset(where)
            )
          }
        }
      }
    }
  }

  private def targetBuildingType = race.detectorBuildingClass

  private def kickOffOn24thTick(): Unit = {
    if (currentTick == 24) {
      analyzed.prepareNextIfDone()
    }
  }

  private def input = new Input

  class Input {
    val limit        = richBaseCount.get min 3
    val created      = currentTick
    val buildingType = targetBuildingType
    val existing     = ownUnits.allByClass(buildingType).map { det =>
      det -> det.detectionArea
    }
    val planned                = unitManager.plannedToBuildByClass(buildingType)
    val exposed                = mapLayers.exposedToCloakedUnits
    val ownArea                = mapLayers.defendedTiles
    val constructionSiteFinder = new ConstructionSiteFinder(universe)

    def age = currentTick - created
  }

}
