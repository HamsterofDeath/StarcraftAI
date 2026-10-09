package pony
package brain
package modules

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

    val cur = plannedSupplies
    val needsMore = cur.supplyUsagePercent >= 0.6 && cur.total < 400

    trace(s"Need more supply: $cur ($plannedSupplies planned)", needsMore)
    if (needsMore) {
      // can't use helper because overlords are not buildings :|
      val result = resources.request(
        ResourceRequests.forUnit(race, classOf[SupplyProvider], Priority.Supply), this)
      result.ifSuccess { suc =>
        trace(s"More supply approved! $suc, requesting ${race.supplyClass.className}")
        val ofType = UnitJobRequest
                     .newOfType(universe, supplyEmployer, classOf[SupplyProvider], suc,
                       priority = Priority.Supply, customBuildingPosition = wallAwareSpot)

        // this will always be unfulfilled
        val result = unitManager.request(ofType)
        assert(!result.success, s"Impossible success: $result")
      }
    }
  }

  private def plannedSupplies = {
    val real = resources.supplies
    val planned = unitManager.plannedSupplyAdditions
    real.copy(total = real.total + planned)
  }
}
