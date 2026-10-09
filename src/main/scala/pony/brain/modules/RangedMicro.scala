package pony
package brain
package modules

import bwapi.{UnitType, WeaponType}
import pony.brain.modules.KitingPolicy._

/**
  * Micro for units with a ranged weapon: focus fire on the weakest enemy in range, and against enemies they outrange
  * "hit, gain distance, repeat". Decides every frame from live native data, because a reload lasts about a game
  * second.
  *
  * `-Dtwailight.kiteShot=stop` fires by stopping and letting the unit acquire a target itself instead of an explicit
  * attack on the focus target; `-Dtwailight.kiteLead=<frames>` sets how early the next attack is ordered;
  * `-Dtwailight.traceKite=true` traces every decision change. Each shot traces the frames between deciding to shoot
  * and the weapon firing.
  */
class RangedMicro(universe: Universe) extends DefaultBehaviour[MobileRangeWeapon](universe) {

  /** Enemies farther away than this (pixels) are left to the other behaviours. */
  private val ConsiderRadius = 12 * 32

  /** Weapons up to this range (pixels) are melee; their units do not get ranged micro. */
  private val MeleeRange = 32

  private val shootByStopping = sys.props.get("twailight.kiteShot").contains("stop")
  private val traceDecisions  = sys.props.get("twailight.traceKite").contains("true")
  private val leadFrames      = sys.props.get("twailight.kiteLead").flatMap(_.toIntOption).filter(_ >= 0).getOrElse(4)

  override def priority = SecondPriority.EvenMore

  override def canControl(u: WrapsUnit) = {
    val kind = u.nativeUnit.getType
    super.canControl(u) && math.max(kind.groundWeapon.maxRange, kind.airWeapon.maxRange) > 2 * MeleeRange
  }

  override protected def wrapBase(unit: MobileRangeWeapon) = new SingleUnitBehaviour[MobileRangeWeapon](unit, meta) {
    private var attached       = false
    private var lastDecision   = ""
    private var shootDecidedAt = Option.empty[Int]
    override def describeShort = "Ranged micro"

    override def toOrder(what: Objective) = {
      val me    = this.unit
      val frame = currentTick
      if (!attached) {
        attached = true
        NativeMatchEvidence.trace("kite-attached", s"unit=${me.nativeUnitId} type=${me.nativeUnit.getType}")
      }
      val targets  = targetsAround(me)
      val shooter  = shooterOf(me, targets.map(_._1))
      val passable = if (me.nativeUnit.isFlying) insideMap else walkable
      val decision = decide(shooter, targets.map(_._2), passable, lead = leadFrames)

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
          val gap = targets.filter(_._2.reach > 0).map(t => shooter.at.distanceTo(t._2.at) - t._2.reach).minOption
          NativeMatchEvidence.trace(
            "kite-decision",
            s"unit=${me.nativeUnitId} decision=$decision cooldown=${shooter.cooldown} firing=${shooter.firing} " +
              s"gap=${gap.map(_.toInt).getOrElse(-1)} order=${me.nativeUnit.getOrder}"
          )
          lastDecision = kind
        }
      }
      decision match {
        case Free                        => Nil
        case Hold                        => Orders.NoUpdate(me).toList
        case Shoot(_) if shootByStopping => Orders.Stop(me).toList
        case Shoot(id)                   => targets.find(_._2.id == id).map(t => Orders.AttackUnit(me, t._1)).toList
        case Retreat(to)                 =>
          Orders.MoveToTile(me, MapTilePosition.shared(to.x.toInt / 32, to.y.toInt / 32)).toList
      }
    }
  }

  private def position(u: WrapsUnit) = Point(u.nativeUnit.getX.toDouble, u.nativeUnit.getY.toDouble)

  private def radius(t: UnitType) = (t.dimensionLeft + t.dimensionRight + 1) / 2.0

  private def weaponAgainst(attacker: UnitType, targetFlies: Boolean) =
    if (targetFlies) attacker.airWeapon else attacker.groundWeapon

  /** The weapon range fits the enemies at hand: ground range when any ground enemy is near, otherwise air range. */
  private def shooterOf(me: MobileRangeWeapon, enemies: Seq[Mobile]) = {
    val native   = me.nativeUnit
    val kind     = native.getType
    val player   = native.getPlayer
    val vsGround = enemies.isEmpty || enemies.exists(!_.nativeUnit.isFlying)
    // weapon ranges are measured between unit edges; 12 pixels stand for a typical target's half width
    Shooter(
      position(me),
      player.weaponMaxRange(weaponAgainst(kind, targetFlies = !vsGround)) + radius(kind) + 12,
      if (vsGround) native.getGroundWeaponCooldown else native.getAirWeaponCooldown,
      native.isAttackFrame,
      player.topSpeed(kind)
    )
  }

  /** Visible, detected enemies within the considered radius that this unit can shoot, with how far they reach it. */
  private def targetsAround(me: MobileRangeWeapon): Vector[(Mobile, Threat)] = {
    val at       = position(me)
    val myKind   = me.nativeUnit.getType
    val myRadius = radius(myKind)
    val iFly     = me.nativeUnit.isFlying
    enemies.allByType[Mobile].iterator.filter { e =>
      val native = e.nativeUnit
      e.isInGame && native.isVisible && native.isDetected && position(e).distanceTo(at) <= ConsiderRadius &&
      weaponAgainst(myKind, native.isFlying) != WeaponType.None
    }.map { e =>
      val native = e.nativeUnit
      val kind   = native.getType
      val weapon = weaponAgainst(kind, iFly)
      val reach  =
        if (weapon == WeaponType.None) 0.0
        else native.getPlayer.weaponMaxRange(weapon) + radius(kind) + myRadius
      e -> Threat(
        native.getID,
        position(e),
        native.getHitPoints + native.getShields,
        reach,
        native.getPlayer.topSpeed(kind)
      )
    }.toVector
  }

  private def insideMap(p: Point) = {
    val grid = mapLayers.rawWalkableMap
    p.x >= 0 && p.y >= 0 && p.x < grid.cols * 32 && p.y < grid.rows * 32
  }

  private def walkable(p: Point) = {
    p.x >= 0 && p.y >= 0 && mapLayers.freeWalkableTiles.containsAndFree(MapTilePosition.shared(
      p.x.toInt / 32,
      p.y.toInt / 32
    ))
  }
}
