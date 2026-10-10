package pony
package brain
package modules
package production

import pony.brain.jobs.Employer
import pony.brain.modules.campaign.CarpetQuotas
import pony.brain.modules.wall.WallWithDepots
import pony.brain.requests.UnitJobRequest
import pony.units.{Factory, Tank}

/** Once the wall stands and tanks exist, factories fly out to the open fields and produce there. */
class FlyFactoriesToNatural(universe: Universe) extends OrderlessAIModule[Factory](universe) {
  private val employers     = new Employer[Factory](universe)
  private var flight        = Option.empty[RelocateFactory]
  private val claimedFields = collection.mutable.Set.empty[Int]

  private def carpet = strategy.current.usesCarpet
  private def wall   = universe.pluginByType[WallWithDepots]

  override def onTick_!(): Unit = {
    if (!carpet || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    flight = flight.filterNot(j => j.failedOrObsolete || j.isFinished)
    if (flight.isDefined) return
    val wallSealed = wall.complete || wall.refused || wall.gateOpen
    val tanks      = ownUnits.allByType[Tank].count(t => t.isInGame && !t.isBeingCreated)
    if (!wallSealed || tanks < CarpetQuotas.tanksBeforeFlight) return
    val homeAreaId = bases.mainBase.map(_.mainBuilding.tilePosition)
    ownUnits.allByType[Factory].filter(f =>
      f.isInGame && !f.isBeingCreated && !f.isFloating &&
        !f.nativeUnit.isTraining && f.nativeUnit.getRemainingTrainTime == 0 &&
        homeAreaId.forall(ht => f.tilePosition.distanceToIsLess(ht, 20)) &&
        unitManager.jobOf(f).isIdle
    )
      .toVector.sortBy(f => (f.tilePosition.y, f.tilePosition.x, f.nativeUnitId))
      .headOption.foreach { factory =>
        val request = UnitJobRequest.idleOfType(employers, classOf[Factory], priority = Priority.Expand)
          .withOnlyAccepting(_.nativeUnitId == factory.nativeUnitId)
        unitManager.request(request).units.headOption.foreach { unit =>
          val job = new RelocateFactory(employers, unit, claimedFields)
          employers.assignJob_!(job)
          flight = Some(job)
          NativeMatchEvidence.trace(
            "factory-flight",
            s"id=${unit.nativeUnitId} from=${unit.tilePosition} tanks=$tanks wallComplete=${wall.complete}"
          )
        }
      }
  }
}
