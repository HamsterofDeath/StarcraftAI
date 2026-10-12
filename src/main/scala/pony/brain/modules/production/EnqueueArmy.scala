package pony
package brain
package modules
package production

import pony.brain.budget.ResourceRequests
import pony.brain.modules.campaign.RunTerranCampaign
import pony.units.{Mobile, UnitFactory}

class EnqueueArmy(universe: Universe)
    extends OrderlessAIModule[UnitFactory](universe) with UnitRequestHelper {

  type Ratio        = (Class[? <: Mobile], Double)
  type RequestOrder = Seq[Ratio]

  private val myRequestPlan = oncePerTick {
    val idealRatios    = percentages
    val mostMissing    = EnqueueArmy.mostMissingFirst(idealRatios.wanted, idealRatios.existing)(_.getName)
    val (now, later)   = mostMissing.partition { case (c, _) => universe.unitManager.allRequirementsFulfilled(c) }
    val needsSomething = later.toSet
    val buildThese     = EnqueueArmy.payableInOrder(now) { e =>
      val rich             = resources.couldAffordNow(e._1)
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
    if (
      race.isTerran && strategy.current.runsTerranCampaign &&
      universe.pluginByType[RunTerranCampaign].holdingNewArmy
    ) return
    val RequestPlan(buildThese, needsSomething) = myRequestPlan.get
    var resourceResults                         = Option.empty[ResourceRequests]
    buildThese.filterNot(needsSomething.contains).foreach { case (thisOne, _) =>
      val needsToSaveMinerals = resourceResults
        .exists(_.minerals > resources.unlockedResources.minerals)
      val needsToSaveGas = resourceResults.exists(_.gas > resources.unlockedResources.gas)
      val required       = ResourceRequests.forUnit(race, thisOne)
      val mineralsGood   = !needsToSaveMinerals
      val gasGood        = !needsToSaveGas || required.gas == 0
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
    val totalWanted       = summed.values.sum
    val percentagesWanted = summed.map { case (t, v) => t -> v.toDouble / totalWanted }

    val existingCounts = {
      val existing = ownUnits.allByType[Mobile].groupBy(_.getClass)
      summed.keySet.map { t =>
        t -> existing.get(t).map(_.size).getOrElse(0)
      }.toMap
    }

    val totalExisting       = existingCounts.values.sum
    val percentagesExisting = existingCounts.map { case (t, v) =>
      t -> (if (totalExisting == 0) 0 else v.toDouble / totalExisting)
    }
    Percentages(percentagesWanted, percentagesExisting)
  }

  case class Percentages(
      wanted: Map[Class[? <: Mobile], Double],
      existing: Map[Class[? <: Mobile], Double]
  )

}

object EnqueueArmy {

  /**
    * The wanted unit types, the furthest below their share first; ties go to the larger share, then by name. The shares
    * are keyed by classes, whose map order changes from run to run.
    */
  def mostMissingFirst[T](wanted: Map[T, Double], existing: Map[T, Double])(name: T => String): Vector[(T, Double)] =
    wanted.toVector.sortBy { case (t, share) => (existing.getOrElse(t, 0.0) / share, -share, name(t)) }

  /**
    * The units to request now among those whose tech stands: in order, up to the first one we cannot pay for yet, so
    * the money is saved for it. A unit still missing its tech must not stop the others: in game 5 vs Zerg on 09c3bb2
    * the finished wall barracks stood idle until zerglings razed it, while a factory unit headed the queue.
    */
  def payableInOrder[T](buildableNow: Vector[T])(payable: T => Boolean): Vector[T] =
    buildableNow.takeWhile(payable)
}
