package pony
package brain
package modules
package micro

import pony.combat.IrradiateChoice
import pony.units.ScienceVessel

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * Science Vessels burn the unit whose whole group takes the most from it (IrradiateChoice): the damage on every
  * organic near the target plus the price share it destroys, less our own units caught in it. With no group in reach
  * the costliest single kill wins. A spot just chosen stays claimed a moment, so two vessels do not burn one group.
  */
class IrradiateUnit(universe: Universe) extends DefaultBehaviour[ScienceVessel](universe) {
  import IrradiateChoice._

  private val claimed = mutable.ArrayBuffer.empty[(Int, Int, Int)]

  override def priority = SecondPriority.Max

  override def refuseCommandsForTicks = 12

  override def onTick_!(): Unit = {
    super.onTick_!()
    claimed.filterInPlace((_, _, at) => currentTick - at < ClaimFrames)
  }

  override protected def wrapBase(unit: ScienceVessel) = new SingleUnitBehaviour[ScienceVessel](unit, meta) {
    override def describeShort = "Irradiate"

    override def preconditionOk = nativeGame.self().hasResearched(bwapi.TechType.Irradiate)

    override protected def toOrder(what: Objective) = {
      val me     = this.unit
      val native = me.nativeUnit
      val tech   = bwapi.TechType.Irradiate
      if (native.getOrder == bwapi.Order.CastIrradiate) List(Orders.NoUpdate(me))
      else if (native.getEnergy < tech.energyCost) Nil
      else {
        val radius              = tech.getWeapon.outerSplashRadius.toDouble
        val reach               = nativeGame.self().weaponMaxRange(tech.getWeapon).toDouble
        val self                = nativeGame.self()
        val around              = native.getUnitsInRadius((reach + radius).toInt).asScala.toVector.filter(_.isVisible)
        def blip(u: bwapi.Unit) = Blip(
          u.getID,
          u.getX.toDouble,
          u.getY.toDouble,
          (u.getHitPoints + u.getShields).toDouble,
          (u.getType.maxHitPoints + u.getType.maxShields).toDouble,
          (u.getType.mineralPrice + u.getType.gasPrice).toDouble,
          u.getPlayer.isEnemy(self),
          u.getType.isOrganic,
          u.isIrradiated
        )
        val pairs   = around.filter(u => u.getPlayer.isEnemy(self) || u.getPlayer == self).map(u => u -> blip(u))
        val blips   = pairs.map(_._2)
        val centres = pairs.collect {
          case (u, b)
              if b.enemy && b.organic && !b.burning &&
                math.hypot(b.x - native.getX, b.y - native.getY) <= reach &&
                !claimed.exists((x, y, _) => math.hypot(b.x - x, b.y - y) < radius) =>
            u -> b
        }
        best(centres.map(_._2), blips, radius).flatMap { (centre, gain) =>
          centres.find(_._2.id == centre.id).map { (target, _) =>
            claimed += ((centre.x.toInt, centre.y.toInt, currentTick))
            NativeMatchEvidence.trace(
              "irradiate",
              s"vessel=${me.nativeUnitId} target=${target.getType} id=${centre.id} score=${gain.round}"
            )
            Orders.UseTechOnUnit(me, target, tech)
          }
        }.toList
      }
    }
  }
}
