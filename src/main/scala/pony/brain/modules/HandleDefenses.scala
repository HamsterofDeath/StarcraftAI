package pony
package brain
package modules

import pony.AttackPriorities.Lowest

class HandleDefenses(universe: Universe) extends OrderlessAIModule[Mobile](universe) {

  private var backgroundOp = BWFuture.none[Seq[Group[Mobile]]]

  override def onTick_!(): Unit = {
    if (race.isTerran && strategy.current.isInstanceOf[Strategy.SimpleTerran]) {
      val localThreats = mapLayers.defendedTiles.allBlocked.flatMap(tile => unitGrid.enemy.onTile(tile))
        .filterNot(_.isHarmlessNow).toVector
      worldDominationPlan.setBaseDefensePressure(localThreats.nonEmpty)
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
      backgroundOp.matchOnOptSelf(groups => {
        debug(s"Attackers grouped")
        val typedGroups = groups.map(GroupingHelper.typedGroup(universe, _))
        //prototype: just attack the biggest group with everything

        val biggest = typedGroups.maxBy(_.members.iterator.map(_.armorType.transportSize).sum)
        worldDominationPlan.initiateAttack(biggest.center, Lowest)

        resetBackgroundOp()
      }, {})
    }
  }

  private def resetBackgroundOp(): Unit = {
    backgroundOp = BWFuture.none
  }
}
