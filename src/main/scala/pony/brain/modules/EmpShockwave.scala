package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * Science Vessels drop EMP Shockwave where it drains the most: shields and energy of the enemy inside the blast,
  * energy weighted higher (a caster's spells cost more than shields), less what it drains of our own units, since
  * EMP spares nobody. Weights from -Dtwailight.empWeights=shields,energy (1,3 by default).
  */
class EmpShockwave(universe: Universe) extends DefaultBehaviour[ScienceVessel](universe) {
  import EmpChoice._

  private val claimed   = mutable.ArrayBuffer.empty[(Int, Int, Int)]
  private val firstSeen = mutable.HashMap.empty[Int, Int]
  private val weights   = sys.props.get("twailight.empWeights").flatMap(parseWeights).getOrElse(DefaultWeights)

  override def priority = SecondPriority.Max

  override def onTick_!(): Unit = {
    super.onTick_!()
    claimed.filterInPlace((_, _, at) => currentTick - at < ClaimFrames)
    val self = nativeGame.self()
    nativeGame.getAllUnits.asScala.iterator.filter(u =>
      u.getPlayer.isEnemy(self) && u.isVisible && u.getType.isSpellcaster
    )
      .foreach(u => firstSeen.getOrElseUpdate(u.getID, currentTick))
  }

  /**
    * The game hides enemy energy: a caster starts with 50 and regains about 0.033 a frame, so it has at most this much
    * since we first saw it, up to its maximum.
    */
  private def energyOf(u: bwapi.Unit, self: bwapi.Player) =
    if (!u.getPlayer.isEnemy(self)) {
      // our own energy matters only where a spell needs it: a cruiser's only with the Yamato gun
      val useless = u.getType == bwapi.UnitType.Terran_Battlecruiser && !self.hasResearched(bwapi.TechType.Yamato_Gun)
      if (useless) 0.0 else u.getEnergy.toDouble
    } else if (!u.getType.isSpellcaster) 0.0
    else EnemyEnergy.estimate(currentTick - firstSeen.getOrElse(u.getID, currentTick), u.getType.maxEnergy)

  override protected def wrapBase(unit: ScienceVessel) = new SingleUnitBehaviour[ScienceVessel](unit, meta) {
    override def describeShort = "EMP"

    override def preconditionOk = nativeGame.self().hasResearched(bwapi.TechType.EMP_Shockwave)

    override protected def toOrder(what: Objective) = {
      val me     = this.unit
      val native = me.nativeUnit
      val tech   = bwapi.TechType.EMP_Shockwave
      if (native.getOrder == bwapi.Order.CastEMPShockwave) List(Orders.NoUpdate(me))
      else if (native.getEnergy < tech.energyCost) Nil
      else {
        val radius              = tech.getWeapon.outerSplashRadius.toDouble
        val reach               = nativeGame.self().weaponMaxRange(tech.getWeapon).toDouble
        val self                = nativeGame.self()
        val around              = native.getUnitsInRadius((reach + radius).toInt).asScala.toVector.filter(_.isVisible)
        def blip(u: bwapi.Unit) =
          Blip(u.getX.toDouble, u.getY.toDouble, u.getShields.toDouble, energyOf(u, self), u.getPlayer.isEnemy(self))
        val blips   = around.filter(u => u.getPlayer.isEnemy(self) || u.getPlayer == self).map(blip)
        val centres = blips.filter(_.enemy).filter(b => math.hypot(b.x - native.getX, b.y - native.getY) <= reach)
          .filterNot(c => claimed.exists((x, y, _) => math.hypot(c.x - x, c.y - y) < radius))
        best(centres, blips, radius, weights).map { (centre, drained) =>
          claimed += ((centre.x.toInt, centre.y.toInt, currentTick))
          NativeMatchEvidence.trace(
            "emp",
            s"vessel=${me.nativeUnitId} at=(${centre.x.toInt / 32},${centre.y.toInt / 32}) score=${drained.round} " +
              s"weights=${weights._1},${weights._2}"
          )
          Orders.UseTechAt(me, centre.x.toInt, centre.y.toInt, tech)
        }.toList
      }
    }
  }
}
