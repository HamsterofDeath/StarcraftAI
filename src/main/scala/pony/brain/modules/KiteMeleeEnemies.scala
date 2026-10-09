package pony
package brain
package modules

import bwapi.{UnitType, WeaponType}
import pony.brain.modules.KitingPolicy._

/**
  * Ranged ground units kite melee enemies: shoot when the weapon is ready, step out of reach while it reloads, repeat.
  * It decides every frame from live native data, because a reload lasts about a game second.
  *
  * `-Dtwailight.kiteShot=stop` fires by stopping and letting the unit acquire a target itself instead of an explicit
  * attack on the focus target; `-Dtwailight.traceKite=true` traces every decision change. Each shot traces how many
  * frames passed between deciding to shoot and the weapon firing.
  */
class KiteMeleeEnemies(universe: Universe) extends DefaultBehaviour[MobileRangeWeapon](universe) {

  /** Enemies farther away than this (pixels) are left to the other behaviours. */
  private val ConsiderRadius = 12 * 32

  /** Ground weapons up to this range (pixels) count as melee. */
  private val MeleeRange = 32

  private val shootByStopping = sys.props.get("twailight.kiteShot").contains("stop")
  private val traceDecisions  = sys.props.get("twailight.traceKite").contains("true")

  override def priority = SecondPriority.EvenMore

  override def canControl(u: WrapsUnit) =
    super.canControl(u) && u.isInstanceOf[GroundUnit] && u.nativeUnit.getType.groundWeapon.maxRange > 2 * MeleeRange

  override protected def wrapBase(unit: MobileRangeWeapon) = new SingleUnitBehaviour[MobileRangeWeapon](unit, meta) {
    private var attached       = false
    private var lastDecision   = ""
    private var shootDecidedAt = Option.empty[Int]
    override def describeShort = "Kite"

    override def toOrder(what: Objective) = {
      val me    = this.unit
      val frame = currentTick
      if (!attached) {
        attached = true
        NativeMatchEvidence.trace("kite-attached", s"unit=${me.nativeUnitId} type=${me.nativeUnit.getType}")
      }
      val threats  = meleeThreatsAround(me)
      val shooter  = shooterOf(me)
      val decision = decide(shooter, threats.map(_._2), walkable)

      shootDecidedAt.foreach { decidedAt =>
        if (shooter.cooldown > 0) {
          NativeMatchEvidence.trace(
            "kite-shot",
            s"unit=${me.nativeUnitId} latency=${frame - decidedAt} by=${if (shootByStopping) "stop" else "attack"}"
          )
          shootDecidedAt = None
        }
      }
      decision match {
        case Shoot(_) if shootDecidedAt.isEmpty => shootDecidedAt = Some(frame)
        case Shoot(_)                           =>
        case _                                  => shootDecidedAt = None
      }
      if (traceDecisions) {
        val kind = decision.getClass.getSimpleName.stripSuffix("$")
        if (kind != lastDecision) {
          val gap = threats.map(t => shooter.at.distanceTo(t._2.at) - t._2.reach).minOption.map(_.toInt)
          NativeMatchEvidence.trace(
            "kite-decision",
            s"unit=${me.nativeUnitId} decision=$decision cooldown=${shooter.cooldown} firing=${shooter.firing} " +
              s"gap=${gap.getOrElse(-1)} order=${me.nativeUnit.getOrder}"
          )
          lastDecision = kind
        }
      }
      decision match {
        case Free                        => Nil
        case Hold                        => Orders.NoUpdate(me).toList
        case Shoot(_) if shootByStopping => Orders.Stop(me).toList
        case Shoot(id)                   => threats.find(_._2.id == id).map(t => Orders.AttackUnit(me, t._1)).toList
        case Retreat(to)                 =>
          Orders.MoveToTile(me, MapTilePosition.shared(to.x.toInt / 32, to.y.toInt / 32)).toList
      }
    }
  }

  private def position(u: WrapsUnit) = Point(u.nativeUnit.getX.toDouble, u.nativeUnit.getY.toDouble)

  private def radius(t: UnitType) = (t.dimensionLeft + t.dimensionRight + 1) / 2.0

  private def shooterOf(me: MobileRangeWeapon) = {
    val native = me.nativeUnit
    val kind   = native.getType
    val player = native.getPlayer
    // weapon ranges are measured between unit edges; 12 pixels stand for a typical melee target's half width
    Shooter(
      position(me),
      player.weaponMaxRange(kind.groundWeapon) + radius(kind) + 12,
      native.getGroundWeaponCooldown,
      native.isAttackFrame,
      player.topSpeed(kind)
    )
  }

  private def meleeThreatsAround(me: MobileRangeWeapon): Vector[(ArmedMobile, Threat)] = {
    val at       = position(me)
    val myRadius = radius(me.nativeUnit.getType)
    enemies.allByType[ArmedMobile].iterator.filter { e =>
      e.isInGame && e.nativeUnit.isVisible && !e.nativeUnit.isFlying && position(e).distanceTo(at) <= ConsiderRadius
    }.flatMap { e =>
      val native = e.nativeUnit
      val kind   = native.getType
      val weapon = kind.groundWeapon
      val reach  = native.getPlayer.weaponMaxRange(weapon)
      if (weapon == WeaponType.None || reach > MeleeRange) None
      else Some(e -> Threat(
        native.getID,
        position(e),
        native.getHitPoints + native.getShields,
        reach + radius(kind) + myRadius,
        native.getPlayer.topSpeed(kind)
      ))
    }.toVector
  }

  private def walkable(p: Point) = {
    p.x >= 0 && p.y >= 0 && mapLayers.freeWalkableTiles.containsAndFree(MapTilePosition.shared(
      p.x.toInt / 32,
      p.y.toInt / 32
    ))
  }
}
