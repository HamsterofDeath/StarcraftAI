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

  private val claimed = mutable.ArrayBuffer.empty[(Int, Int, Int)]
  private val weights = sys.props.get("twailight.empWeights").flatMap(parseWeights).getOrElse(DefaultWeights)

  override def priority = SecondPriority.Max

  override def onTick_!(): Unit = {
    super.onTick_!()
    claimed.filterInPlace((_, _, at) => currentTick - at < ClaimFrames)
  }

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
          Blip(u.getX.toDouble, u.getY.toDouble, u.getShields.toDouble, u.getEnergy.toDouble, u.getPlayer.isEnemy(self))
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

/** Where an EMP drains the most, kept free of the game; pixels. */
private[pony] object EmpChoice {

  /** A unit as the blast sees it: where it stands, its shields and energy, and whose it is. */
  final case class Blip(x: Double, y: Double, shields: Double, energy: Double, enemy: Boolean)

  /** Shields count once, energy three times: what casters lose weighs more than what shields regrow. */
  val DefaultWeights = (1.0, 3.0)

  /** Below this a blast is not worth its hundred energy. */
  val MinScore = 150.0

  /** Another Vessel leaves a spot just chosen alone this long. */
  val ClaimFrames = 48

  def parseWeights(text: String): Option[(Double, Double)] = text.split(',').map(_.trim.toDoubleOption) match {
    case Array(Some(s), Some(e)) => Some((s, e))
    case _                       => None
  }

  /** What a blast at `centre` drains of the enemy, less what it drains of our own units. */
  def score(centre: Blip, units: Seq[Blip], radius: Double, weights: (Double, Double)): Double =
    units.filter(u => math.hypot(u.x - centre.x, u.y - centre.y) <= radius).map { u =>
      val drained = u.shields * weights._1 + u.energy * weights._2
      if (u.enemy) drained else -drained
    }.sum

  def best(
      centres: Seq[Blip],
      units: Seq[Blip],
      radius: Double,
      weights: (Double, Double)
  ): Option[(Blip, Double)] =
    centres.map(c => c -> score(c, units, radius, weights)).filter(_._2 >= MinScore).maxByOption(_._2)
}
