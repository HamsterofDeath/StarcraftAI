package pony
package brain
package modules
package campaign

import pony.units.{Mobile, WorkerUnit, WrapsUnit}
import pony.util.BWFuture

import pony.combat.AttackPriorities.Lowest

class HandleDefenses(universe: Universe) extends OrderlessAIModule[Mobile](universe) {

  private var backgroundOp = BWFuture.none[Seq[Group[Mobile]]]

  override def onTick_!(): Unit = {
    if (race.isTerran && strategy.current.runsTerranCampaign) {
      val localThreats = mapLayers.defendedTiles.allBlocked.flatMap(tile => unitGrid.enemy.onTile(tile))
        .filterNot(_.isHarmlessNow).toVector.distinctBy(_.nativeUnitId)
      if (localThreats.isEmpty) worldDominationPlan.setBaseDefensePressure(false)
      else {
        def value(u: WrapsUnit) = u.nativeUnitType.mineralPrice + u.nativeUnitType.gasPrice
        val out                 = worldDominationPlan.campaignForce ++ worldDominationPlan.raidingFleet
        // behind a sealed wall the ground units cannot reach a raid outside it, nor help against it
        val raided = localThreats.head.currentTile
        val home   = ownUnits.allMobilesWithWeapons.iterator.filter { m =>
          m.isInGame && !m.isBeingCreated && m.isFigher && !m.isInstanceOf[WorkerUnit] && !out(m) &&
          (m.nativeUnit.isFlying || !ferryManager.sealedApart(m.currentTile, raided))
        }.map(value).sum
        val threat   = localThreats.iterator.map(value).sum
        val campaign = out.iterator.map(value).sum
        worldDominationPlan.setBaseDefensePressure(
          true,
          RaidResponse.recallsCampaign(threat, home, campaign),
          s"threat=$threat home=$home campaign=$campaign"
        )
      }
      localThreats.headOption.foreach(threat => worldDominationPlan.requestBaseDefense(threat.currentTile))
      return
    }
    if (backgroundOp.result.isEmpty) {
      ifNth(Primes.prime43) {
        val allEnemies = mapLayers.defendedTiles.allBlocked.flatMap { tile =>
          unitGrid.enemy.onTile(tile)
        }.filterNot(_.isHarmlessNow)

        if (allEnemies.nonEmpty) {
          debug(s"Enemy detected! Calculating counterattack")
          val newBackgroundOp = GroupingHelper.groupThese(allEnemies, universe)
          backgroundOp = newBackgroundOp
        }
      }
    } else {
      backgroundOp.matchOnOptSelf(
        groups => {
          debug(s"Attackers grouped")
          val typedGroups = groups.map(GroupingHelper.typedGroup(universe, _))
          // prototype: just attack the biggest group with everything

          val biggest = typedGroups.maxBy(_.members.iterator.map(_.armorType.transportSize).sum)
          worldDominationPlan.initiateAttack(biggest.center, Lowest)

          resetBackgroundOp()
        },
        {}
      )
    }
  }

  private def resetBackgroundOp(): Unit = {
    backgroundOp = BWFuture.none
  }
}
