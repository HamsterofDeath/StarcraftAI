package pony
package brain
package modules
package micro

import pony.units.Tank

import pony.tech.Upgrades.Terran._

import scala.reflect.ClassTag

class SiegeUnsiegeSelf(universe: Universe) extends DefaultBehaviour[Tank](universe) {

  override def forceRepeatedCommands = true

  override def priority = SecondPriority.Max

  override protected def wrapBase(unit: Tank) = new SingleUnitBehaviour[Tank](unit, meta) {

    override def preconditionOk = upgrades.hasResearched(TankSiegeMode)

    private var wasSieged           = this.unit.isSieged
    private var siegeOrderAnnounced = false

    override def toOrder(what: Objective) = {
      if (this.unit.isSieged != wasSieged) {
        siegeOrderAnnounced = false
        wasSieged = this.unit.isSieged
        NativeMatchEvidence.trace(
          "siege-change",
          s"unit=${this.unit.nativeUnitId} sieged=$wasSieged at=${this.unit.currentTile}"
        )
      }
      val enemies = this.unit.surroundings.mediumEnemyGroundUnits
      // a lifted Terran building is out of a tank's reach and must not hold it sieged over a razed main
      val buildings = this.unit.surroundings.mediumEnemyBuildings.filterNot(_.isFloating)

      def buildingInRange = buildings.exists(_.area.distanceTo(this.unit.currentTile) <= 12)

      def anyRangedEnemy =
        enemies.exists(e => !e.isHarmlessNow && e.nativeUnit.getType.groundWeapon.maxRange > 32)

      // annoy: a single ranged enemy inside siege reach that the tank outranges is free damage once sieged
      def outrangedInSiegeReach =
        enemies.exists { e =>
          !e.isHarmlessNow && e.nativeUnit.getType.groundWeapon.maxRange > 32 &&
          !e.centerTile.distanceToIsMore(this.unit.centerTile, 12) &&
          e.centerTile.distanceToIsMore(this.unit.centerTile, 2)
        }

      def siegeableInRange = {
        buildingInRange || (anyRangedEnemy && enemies.iterator.filterNot(_.isHarmlessNow).take(2).size >= 2)
      }

      def anyCloseButNotTooClose = {
        enemies.exists { e =>
          !e.isHarmlessNow && e.centerTile.distanceToIsMore(this.unit.centerTile, 2)
        }
      }

      val botheredByMelee = this.unit.underAttackByMelee
      if (this.unit.isSieged) {
        val staySieged = {
          val hasTargets = buildingInRange || anyCloseButNotTooClose
          hasTargets
        }
        if (staySieged) {
          Nil
        } else {
          Orders.TechOnSelf(this.unit, TankSiegeMode).toList
        }
      } else {
        if (buildingInRange || ((siegeableInRange || outrangedInSiegeReach) && !botheredByMelee)) {
          if (!siegeOrderAnnounced) {
            siegeOrderAnnounced = true
            NativeMatchEvidence.trace(
              "siege-order",
              s"unit=${this.unit.nativeUnitId} buildings=${buildings.size} buildingInRange=$buildingInRange " +
                s"enemies=${enemies.size} ranged=$anyRangedEnemy annoy=$outrangedInSiegeReach at=${this.unit.currentTile}"
            )
          }
          Orders.TechOnSelf(this.unit, TankSiegeMode).toList
        } else {
          Nil
        }
      }
    }

    override def describeShort: String = s"Siegemode"
  }
}
