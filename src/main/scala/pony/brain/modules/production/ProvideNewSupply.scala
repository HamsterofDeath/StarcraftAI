package pony
package brain
package modules
package production

import pony.brain.budget.ResourceRequests
import pony.brain.jobs.Employer
import pony.brain.modules.wall.WallWithDepots
import pony.brain.requests.UnitJobRequest
import pony.units.{SupplyProvider, WorkerUnit}

class ProvideNewSupply(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) {
  private val supplyEmployer = new Employer[SupplyProvider](universe)

  private def wall = universe.plugins.collectFirst { case w: WallWithDepots => w }

  /** The first supply depots stand at the wall so the opening's depots double as the wall. */
  private def wallAwareSpot: AlternativeBuildingSpot = wall.flatMap(_.nextSupplySpot) match {
    case Some(spot) =>
      AlternativeBuildingSpot.fromValidatedPreset(spot)(wall.exists(_.supplySpotValid(spot)))
    case None => AlternativeBuildingSpot.useDefault
  }

  override def onTick_!() = {

    val cur       = plannedSupplies
    val needsMore = cur.supplyUsagePercent >= ProvideNewSupply.requestAt(
      wall.exists(_.nextSupplySpot.isDefined),
      forces.isTvZ
    ) && cur.total < 400

    trace(s"Need more supply: $cur ($plannedSupplies planned)", needsMore)
    if (needsMore) {
      // can't use helper because overlords are not buildings :|
      val result = resources.request(
        ResourceRequests.forUnit(race, classOf[SupplyProvider], Priority.Supply),
        this
      )
      result.ifSuccess { suc =>
        trace(s"More supply approved! $suc, requesting ${race.supplyClass.className}")
        val ofType = UnitJobRequest
          .newOfType(
            universe,
            supplyEmployer,
            classOf[SupplyProvider],
            suc,
            priority = Priority.Supply,
            customBuildingPosition = wallAwareSpot
          )

        // this will always be unfulfilled
        val result = unitManager.request(ofType)
        assert(!result.success, s"Impossible success: $result")
      }
    }
  }

  private def plannedSupplies = {
    val real    = resources.supplies
    val planned = unitManager.plannedSupplyAdditions
    real.copy(total = real.total + planned)
  }
}

object ProvideNewSupply {

  /**
    * The supply usage at which the next depot is requested. Against Zerg a wall still missing depots asks earlier: in
    * game 1 vs Zerg on 34fb4c2 zerglings came at frame 3745 through the slot of the third wall depot, which sixty
    * percent asked for only at frame 3976, and killed every SCV.
    */
  def requestAt(wallMissingDepots: Boolean, vsZerg: Boolean): Double = if (wallMissingDepots && vsZerg) 0.4 else 0.6
}
