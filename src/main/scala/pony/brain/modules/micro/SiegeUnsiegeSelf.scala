package pony
package brain
package modules
package micro

import pony.units.Tank

import pony.tech.Upgrades.Terran._

import scala.reflect.ClassTag

class SiegeUnsiegeSelf(universe: Universe) extends DefaultBehaviour[Tank](universe) {

  override def forceRepeatedCommands = true

  override def priority = SecondPriority.EvenMore

  override protected def wrapBase(unit: Tank) = new SingleUnitBehaviour[Tank](unit, meta) {

    override def preconditionOk = upgrades.hasResearched(TankSiegeMode)

    override def toOrder(what: Objective) = {
      val enemies   = this.unit.surroundings.mediumEnemyGroundUnits
      val buildings = this.unit.surroundings.mediumEnemyBuildings

      def buildingInRange = buildings.exists(_.area.distanceTo(this.unit.currentTile) <= 11)

      def siegeableInRange = {
        buildingInRange || enemies.iterator.filterNot(_.isHarmlessNow).take(4).size >= 3
      }

      def anyCloseButNotTooClose = {
        enemies.exists { e =>
          !e.isHarmlessNow && e.centerTile.distanceToIsMore(this.unit.centerTile, 4)
        }
      }

      val botheredByMelee = this.unit.underAttackByMelee
      if (this.unit.isSieged) {
        val staySieged = {
          val hasTargets = buildingInRange || anyCloseButNotTooClose
          hasTargets && !botheredByMelee
        }
        if (staySieged) {
          Nil
        } else {
          Orders.TechOnSelf(this.unit, TankSiegeMode).toList
        }
      } else {
        if (siegeableInRange && !botheredByMelee) {
          Orders.TechOnSelf(this.unit, TankSiegeMode).toList
        } else {
          Nil
        }
      }
    }

    override def describeShort: String = s"Siegemode"
  }
}
