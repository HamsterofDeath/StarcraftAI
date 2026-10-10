package pony
package brain
package modules
package economy

import pony.brain.budget.ResourceRequests
import pony.brain.modules.campaign.TerranCampaignConfig
import pony.brain.modules.production.{AlternativeBuildingSpot, BuildingRequestHelper}
import pony.render.Renderer
import pony.terrain.{ConstructionSiteFinder, MineralPatchGroup, ResourceArea}
import pony.units.{MainBuilding, Mobile, WorkerUnit}

import bwapi.Color

class ProvideExpansions(universe: Universe)
    extends OrderlessAIModule[WorkerUnit](universe) with BuildingRequestHelper {
  private var plannedExpansionPoint = Option.empty[ResourceArea]
  private lazy val terranOpening    = new TerranEconomicOpening(universe)

  def forceExpand(patch: MineralPatchGroup) = {
    plannedExpansionPoint = {
      val all = world.resourceAnalyzer.resourceAreas
      all.find(_.isPatchId(patch.patchId))
    }
  }

  override def renderDebug(renderer: Renderer) = {
    plannedExpansionPoint.foreach { res =>
      renderer.in_!(Color.White).drawTextAtTile("   Ex", res.center)
    }
  }

  override def onTick_!(): Unit = {
    if (race.isTerran && strategy.current.runsTerranCampaign) {
      terranOpening.onTick_!()
      return
    }
    ifNth(Primes.prime31) {
      plannedExpansionPoint = plannedExpansionPoint.filter { where =>
        universe.mapLayers.slightlyDangerousAsBlocked.free(where.center) &&
        universe.unitGrid.enemy.allInRange[Mobile](where.center, 12).isEmpty &&
        !bases.isCovered(where)
      }.orElse(strategy.current.suggestNextExpansion)
      info(s"AI wants to expand to ${plannedExpansionPoint.get}", plannedExpansionPoint.isDefined)
    }

    plannedExpansionPoint.filter { _ =>
      // only build one at a time
      !unitManager.requestedToBuild(race.resourceDepositClass) &&
      unitManager.constructionsInProgress[MainBuilding].isEmpty
    }.foreach { resources =>
      val plannedMainBuildings = unitManager.constructionsInProgress(race.resourceDepositClass)
      plannedMainBuildings.find(_.belongsTo.contains(resources)) match {
        case Some(planned) =>
          if (planned.building.isDefined) {
            trace(s"New expansion exists, resetting expansion plans")
            plannedExpansionPoint = None
          }
        case None =>
          val cost = ResourceRequests.forUnit(race, race.resourceDepositClass)
          val safe = universe.mapLayers.slightlyDangerousAsBlocked.free(resources.center) &&
            universe.unitGrid.enemy.allInRange[Mobile](resources.center, 12).isEmpty &&
            !bases.isCovered(resources) && bases.mainBase.exists { base =>
              mapLayers.rawWalkableMap.areInSameWalkableArea(resources.nearbyFreeTile, base.mainBuilding.tilePosition)
            }
          val funds = universe.resources.unlockedResources
          if (
            !unitManager.requestedToBuild(race.resourceDepositClass) &&
            TerranCampaignConfig.load().expand(
              funds.minerals,
              funds.gas,
              cost.minerals,
              cost.gas,
              pending = false,
              safeReachableSite = safe
            )
          ) {
            val buildingSpot = AlternativeBuildingSpot
              .fromExpensive(
                new ConstructionSiteFinder(universe).forResourceArea(resources)
              )(
                _.find
              )
            requestBuilding(
              race.resourceDepositClass,
              takeCareOfDependencies = false,
              saveMoneyIfPoor = false,
              buildingSpot,
              belongsTo = plannedExpansionPoint,
              priority = Priority.Expand
            )
            NativeMatchEvidence.trace("expansion-request", s"${resources.center} unlocked=${funds.minerals}")
          }
      }
    }
  }
}
