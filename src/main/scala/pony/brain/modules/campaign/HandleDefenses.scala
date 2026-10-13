package pony
package brain
package modules
package campaign

import pony.brain.modules.wall.WallWithDepots
import pony.geometry.MapTilePosition
import pony.terrain.MapLayers
import pony.units.{Building, Mobile, WorkerUnit, WrapsUnit}
import pony.util.BWFuture

import pony.combat.AttackPriorities.Lowest

class HandleDefenses(universe: Universe) extends OrderlessAIModule[Mobile](universe) {

  private var backgroundOp = BWFuture.none[Seq[Group[Mobile]]]

  override def onTick_!(): Unit = {
    if (race.isTerran && strategy.current.runsTerranCampaign) {
      val localThreats = (mapLayers.defendedTiles.allBlocked.flatMap(tile => unitGrid.enemy.onTile(tile)) ++ shelling)
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
        val by       = localThreats.groupBy(_.nativeUnitType.toString.replaceAll("^[A-Za-z]+_", "")).toVector
          .map((k, v) => s"$k*${v.size}").sorted.mkString(",")
        worldDominationPlan.setBaseDefensePressure(
          true,
          RaidResponse.recallsCampaign(threat, home, campaign),
          s"threat=$threat home=$home campaign=$campaign at=$raided by=$by"
        )
        val short = RaidResponse.detachment(threat, home, campaign)
        if (short > 0) {
          val sent = worldDominationPlan.detachFromCampaign_!(raided, short, value)
          if (sent.nonEmpty) NativeMatchEvidence.trace(
            "base-defense-detach",
            s"at=$raided worth=${sent.map(value).sum} units=${sent.size} threat=$threat home=$home campaign=$campaign"
          )
        }
        val force = forceBeyondWall(localThreats, raided, value)
        val hold  = RaidResponse.holdsBehindWall(force, home, holding)
        if (hold != holding) NativeMatchEvidence.trace(
          "base-defense-hold",
          s"${if (hold) "on" else "off"} at=$raided force=${force.getOrElse(0)} home=$home threat=$threat"
        )
        holding = hold
        if (hold) worldDominationPlan.holdBaseDefense_!()
        else worldDominationPlan.requestBaseDefense(raided)
      }
      if (localThreats.isEmpty) holding = false
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

  private var holding = false

  /** How far from the raid enemies count as its force: the twelve zealots of game 1 stood within ten tiles. */
  private val HoldRadius = 12

  /**
    * The worth of the visible enemy ground force near a raid that stands wholly beyond our closed wall, or None for a
    * raid the wall does not keep out: fliers, guns that reach past the defended ring, a sealed main (its fighters cannot
    * get out), or any raider inside. Home fighters sent beyond the wall walk out through a slit only a Marine fits, one
    * by one: in game 1 vs Protoss on ICCup Lost_Temple at 87b3736 the first of twelve zealots at the wall counted 100
    * against 600 at home, and eleven Marines went out to it and died. Held, they shoot over the wall from behind it.
    */
  private def forceBeyondWall(raiders: Seq[Mobile], raided: MapTilePosition, value: WrapsUnit => Int): Option[Int] = {
    val wall   = universe.pluginByType[WallWithDepots]
    val beyond = !strategy.current.sealsMain && raiders.forall { e =>
      !e.nativeUnit.isFlying && e.nativeUnit.getType.groundWeapon.maxRange / 32 <= MapLayers.DefendedRing &&
      wall.closedAgainst(e.currentTile)
    }
    Option.when(beyond) {
      enemies.allCompletedMobiles.iterator.filter { e =>
        e.nativeUnit.isVisible && !e.nativeUnit.isFlying && !e.isInstanceOf[WorkerUnit] && !e.isHarmlessNow &&
        e.currentTile.distanceToIsLess(raided, HoldRadius)
      }.map(value).sum
    }
  }

  /**
    * Visible enemies whose reach exceeds the defended ring around our buildings and takes in one of them: a sieged
    * tank shells a base from twelve tiles, beyond the ring and mostly beyond our sight. In game 4 vs Terran on d96d96e
    * such tanks took the workers from 50 to 28 while the raid counted 50.
    */
  private def shelling = {
    val buildings = ownUnits.allByType[Building].iterator.filterNot(_.isFloating).map(_.centerTile).toVector
    enemies.allCompletedMobiles.iterator.filter { e =>
      val reach = e.nativeUnit.getType.groundWeapon.maxRange / 32
      reach > MapLayers.DefendedRing && e.nativeUnit.isVisible &&
      buildings.exists(_.distanceToIsLess(e.currentTile, reach + 2))
    }
  }
}
