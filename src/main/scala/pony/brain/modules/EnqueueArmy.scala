package pony
package brain
package modules

class EnqueueArmy(universe: Universe)
  extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {

  type Ratio = (Class[? <: Mobile], Double)
  type RequestOrder = Seq[Ratio]

  private val myRequestPlan = oncePerTick {
    val idealRatios = percentages
    val mostMissing = idealRatios.wanted.toVector.sortBy { case (t, idealRatio) =>
      val existingRatio = idealRatios.existing.getOrElse(t, 0.0)
      existingRatio / idealRatio
    }
    val needsSomething = {
    val (_, later) = mostMissing.partition { case (c, _) => universe.unitManager.allRequirementsFulfilled(c) }
      later.toSet
    }

    val canBuildNow = mostMissing.filterNot {
      case (c, _) => universe.unitManager.requirementsQueuedToBuild(c)
    }
    val highestPriority = canBuildNow
    val buildThese =
      highestPriority.takeWhile { e =>
        val rich = resources.couldAffordNow(e._1)
        def mineralsOverflow = resources.unlockedResources.moreMineralsThanGas &&
                               resources.unlockedResources.minerals > 400
        def gasOverflow = resources.unlockedResources.moreGasThanMinerals &&
                          resources.unlockedResources.gas > 400
        rich || mineralsOverflow || gasOverflow
      }
    RequestPlan(buildThese, needsSomething)
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (race.isTerran && strategy.current.isInstanceOf[Strategy.SimpleTerran] &&
      universe.pluginByType[RunTerranCampaign].holdingNewArmy) return
    val RequestPlan(buildThese, needsSomething) = myRequestPlan.get
    var resourceResults = Option.empty[ResourceRequests]
    buildThese.filterNot(needsSomething.contains).foreach { case (thisOne, _) =>
      val needsToSaveMinerals = resourceResults
                                .exists(_.minerals > resources.unlockedResources.minerals)
      val needsToSaveGas = resourceResults.exists(_.gas > resources.unlockedResources.gas)
      val required = ResourceRequests.forUnit(race, thisOne)
      val mineralsGood = !needsToSaveMinerals
      val gasGood = !needsToSaveGas || required.gas == 0
      if (mineralsGood && gasGood) {
        val accepted = requestUnit(thisOne, takeCareOfDependencies = false)
        if (!accepted) {
          resourceResults = resourceResults.fold(required)(_ + required).toSome
        }
      }
    }

    needsSomething.foreach { case (thisOne, _) =>
      requestUnit(thisOne, takeCareOfDependencies = true)
    }
  }

  def plan = myRequestPlan.get

  def percentages: Percentages = {
    val ratios = strategy.current
                 .suggestUnits
                 .filter(_.isActive)
    val summed = {
      ratios.groupBy(_.unitType)
      .map { case (t, v) =>
        (t, v.map(_.fixedAmount).sum)
      }
    }
    val totalWanted = summed.values.sum
    val percentagesWanted = summed.map { case (t, v) => t -> v.toDouble / totalWanted }

    val existingCounts = {
      val existing = ownUnits.allByType[Mobile].groupBy(_.getClass)
      summed.keySet.map { t =>
        t -> existing.get(t).map(_.size).getOrElse(0)
      }.toMap
    }

    val totalExisting = existingCounts.values.sum
    val percentagesExisting = existingCounts.map { case (t, v) =>
      t -> (if (totalExisting == 0) 0 else v.toDouble / totalExisting)
    }
    Percentages(percentagesWanted, percentagesExisting)
  }

  case class Percentages(wanted: Map[Class[? <: Mobile], Double],
                         existing: Map[Class[? <: Mobile], Double])

}
