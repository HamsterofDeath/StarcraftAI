package pony
package brain
package modules

import bwapi.Color

import scala.reflect.ClassTag

class MigrateTowardsPosition(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
  override def priority = SecondPriority.Less

  override def canControl(u: WrapsUnit): Boolean = {
    super.canControl(u) && u.isFigher
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
  }

  override def renderDebug_!(renderer: Renderer) = {
    super.renderDebug_!(renderer)
    worldDominationPlan.allAttacks.foreach { plan =>
      plan.migrationPlan.foreach { p =>
        p.targetFormationTiles.foreach { tile =>
          renderer.in_!(Color.Cyan).drawCircleAround(tile)
        }
        renderer.in_!(Color.Red).drawStar(p.originalDestination, 3)
        renderer.in_!(Color.Green).drawStar(p.safeDestination, 2)
      }
    }
  }

  override protected def wrapBase(unit: Mobile): SingleUnitBehaviour[Mobile] = {
    def finalDestination = {
      worldDominationPlan.attackOf(unit).map { attack =>
        attack.destination.where
      }
    }
    val behaviour = unit match {
      case g: GroundUnit =>
        new DefaultMigrationBehaviour[GroundUnit](g) with FerrySupport[GroundUnit] {
          override protected def ferryDropTarget = {
            finalDestination
          }
        }

      case m => new DefaultMigrationBehaviour[Mobile](unit)
    }
    behaviour
  }

  private class DefaultMigrationBehaviour[+T <: Mobile](unit: T)
      extends SingleUnitBehaviour(unit, meta) {
    override def describeShort = "--> X"

    override protected def toOrder(what: Objective) = {
      val campaign   = this.universe.pluginByType[RunTerranCampaign]
      val guardOrder = campaign.guardPosition(unit).filter(p =>
        unit.currentTile.distanceToIsMore(p, 5) && worldDominationPlan.attackOf(unit).isEmpty
      )
        .map(p => Orders.AttackMove(unit, p))
      worldDominationPlan.immediateBaseDefenseOrder(unit).orElse(guardOrder).map(_.toList).getOrElse(
        worldDominationPlan.attackOf(unit).map { attack =>
          attack.suggestActionFor(unit).asOrder.toList
        }.getOrElse(Nil)
      )
    }
  }

}
