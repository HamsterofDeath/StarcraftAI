package pony
package brain
package modules

class ProvideUpgrades(universe: Universe) extends OrderlessAIModule[Upgrader](universe) {
  self =>
  private val helper     = new HelperAIModule[WorkerUnit](universe) with BuildingRequestHelper
  private val researched = collection.mutable.Map.empty[Upgrade, Int]
  private val inProgress = collection.mutable.Set.empty[Upgrade]

  override def onTick_!(): Unit = {
    val maxLimitEnabled = hasLimitDisabler
    val requested       = {
      strategy.current.suggestUpgrades
        .filterNot(e => researched.getOrElse(e.upgrade, 0) == maxLimitEnabled.ifElse(e.maxLevel, 1))
        .filterNot(e => inProgress.contains(e.upgrade))
        .filter(_.isActive)
    }

    requested.foreach { request =>
      val wantedUpgrade           = request.upgrade
      val needs                   = race.techTree.upgraderFor(wantedUpgrade)
      val buildingPlannedOrExists = unitManager.existsOrPlanned(needs)
      if (buildingPlannedOrExists) {
        val buildMissing = !unitManager.requestedToBuild(needs) &&
          ownUnits.allByClass(needs).size < bases.richBases.size

        val result = unitManager
          .request(UnitJobRequest.upgraderFor(wantedUpgrade, self), buildMissing)
        result.units.foreach { up =>
          val price = new UpgradePrice {
            private val current = researched.getOrElse(wantedUpgrade, 0)

            override def nextMineralPrice = wantedUpgrade.mineralPriceForStep(current)

            override def forUpgrade = wantedUpgrade

            override def nextGasPrice = wantedUpgrade.gasPriceForStep(current)
          }
          val result = resources.request(ResourceRequests.forUpgrade(up, price), self)
          result.ifSuccess { app =>
            info(s"Starting research of $wantedUpgrade")
            val researchUpgrade = new ResearchUpgrade(self, up, wantedUpgrade, app)
            inProgress += wantedUpgrade
            researchUpgrade.listen_!(failed => {
              inProgress -= wantedUpgrade
              if (!failed) {
                info(s"Research of $wantedUpgrade completed")
                val current = researched.getOrElse(wantedUpgrade, 0)
                researched.put(wantedUpgrade, current + 1)
                upgrades.notifyResearched_!(wantedUpgrade)
              }
            })
            assignJob_!(researchUpgrade)
          }
        }
      } else {
        trace(s"Requesting ${
            needs.className
          } to be build in order for $wantedUpgrade to be researched")
        helper.requestBuilding(needs, takeCareOfDependencies = true)
      }
    }
  }

  private def hasLimitDisabler = universe.ownUnits.allByType[UpgradeLimitLifter].nonEmpty
}
