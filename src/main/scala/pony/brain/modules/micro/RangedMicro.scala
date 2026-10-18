package pony
package brain
package modules
package micro

import pony.combat.{ArmedBuilding, MobileRangeWeapon}
import pony.geometry.MapTilePosition
import pony.units.{CanDie, Mobile, SCV, Tank, WrapsUnit}

import bwapi.{UnitType, WeaponType}
import pony.brain.modules.micro.KitingPolicy._

import scala.collection.mutable

/**
  * Micro for units with a ranged weapon: focus fire on the weakest enemy in range, against enemies they outrange "hit,
  * gain distance, repeat", and against static defence "fire, go back" for the unit it shoots at. Decides every frame
  * from live native data, because a reload lasts about a game second.
  *
  * `-Dtwailight.kiteShot=stop` fires by stopping and letting the unit acquire a target itself instead of an explicit
  * attack on the focus target; `-Dtwailight.kiteLead=<frames>` sets how early the next attack is ordered;
  * `-Dtwailight.overwhelmRatio=<x>` sets the damage ratio at which a flight stops passing as a group;
  * `-Dtwailight.turretDance=all|off` sends every reloading unit or none out of the reach of static defence instead of
  * the unit it shoots at; `-Dtwailight.traceKite=true` traces every decision change. Each shot traces the frames between deciding to shoot
  * and the weapon firing.
  */
class RangedMicro(universe: Universe) extends DefaultBehaviour[MobileRangeWeapon](universe) {

  /** Enemies farther away than this (pixels) are left to the other behaviours. */
  private val ConsiderRadius = 12 * 32

  /** Weapons up to this range (pixels) are melee; their units do not get ranged micro. */
  private val MeleeRange = 32

  /** Edge-to-edge distance unsieged tanks start keeping from melee enemies while they reload. */
  private val TankMeleeStandoff = 160.0

  private val shootByStopping = sys.props.get("twailight.kiteShot").contains("stop")
  private val traceDecisions  = sys.props.get("twailight.traceKite").contains("true")
  private val focusMode       = sys.props.get("twailight.focusFire").map(FocusMode.parse).getOrElse(FocusMode.Threat)
  private val leadFrames      = sys.props.get("twailight.kiteLead").flatMap(_.toIntOption).filter(_ >= 0).getOrElse(4)
  private val speedRatio      =
    sys.props.get("twailight.kiteSpeedRatio").flatMap(_.toDoubleOption).filter(_ > 0).getOrElse(1.25)
  private val overwhelmRatio =
    sys.props.get("twailight.overwhelmRatio").flatMap(_.toDoubleOption).filter(_ > 0).getOrElse(OverwhelmRatio)
  private val dance = sys.props.get("twailight.turretDance") match {
    case Some("all") => Dance.All
    case Some("off") => Dance.Off
    case _           => Dance.Aimed
  }

  /** Damage every shooter has committed to each enemy id in the current frame, shared so they do not overkill. */
  private val committedDamage = mutable.HashMap.empty[Int, Double]
  private var commitmentFrame = -1

  /** The shared pass rhythm against static defence (KitingPolicy.nextPassPhase), owned here for the whole flight. */
  private var passPhase: PassPhase = PassPhase.Forming
  private var passFrame            = -1
  private var passSince            = 0
  private val firedThisPass        = mutable.HashSet.empty[Int]
  private var passTurret           = Option.empty[(Point, Double)]

  private def commitments(frame: Int) = {
    if (commitmentFrame != frame) {
      committedDamage.clear()
      commitmentFrame = frame
    }
    committedDamage
  }

  private def badlyHurt(n: bwapi.Unit) =
    (n.getHitPoints + n.getShields) * 4 < (n.getType.maxHitPoints + n.getType.maxShields) * 3

  /**
    * Advance the shared pass phase once per frame from the flight around the current static target, and hand
    * back the phase for this frame. Members are the flyers within reach of the fight; support means an SCV
    * waits close enough to mend them.
    */
  private def updatePass(frame: Int, caller: MobileRangeWeapon, threats: Seq[Threat]): PassPhase = {
    threats.filter(t => t.speed == 0 && t.reach > 0).minByOption(t => position(caller).distanceTo(t.at))
      .foreach(t => passTurret = Some(t.at -> t.reach))
    if (passFrame != frame) {
      passFrame = frame
      passTurret.foreach { (at, reach) =>
        val support = ownUnits.allByType[SCV]
          .exists(s => s.isInGame && !s.isBeingCreated && position(s).distanceTo(at) <= 24 * 32)
        val members = ownUnits.allByType[MobileRangeWeapon].filter { a =>
          a.exists && a.nativeUnit.isFlying && position(a).distanceTo(at) <= 48 * 32
        }.map { a =>
          PassMember(
            inside = position(a).distanceTo(at) - reach < StandOff,
            ready = a.nativeUnit.getGroundWeaponCooldown <= leadFrames,
            hurt = badlyHurt(a.nativeUnit),
            fired = firedThisPass.contains(a.nativeUnitId)
          )
        }.toSeq
        val next = nextPassPhase(passPhase, members, support, frame - passSince)
        if (next != passPhase) {
          if (traceDecisions)
            NativeMatchEvidence.trace(
              "kite-pass",
              s"phase=$passPhase->$next members=${members.size} of=${ownUnits.allByType[MobileRangeWeapon].size} support=$support"
            )
          if (next == PassPhase.Diving) firedThisPass.clear()
          passPhase = next
          passSince = frame
        }
      }
    }
    passPhase
  }

  override def priority = SecondPriority.EvenMore

  override def canControl(u: WrapsUnit) = {
    val kind = u.nativeUnit.getType
    super.canControl(u) && math.max(kind.groundWeapon.maxRange, kind.airWeapon.maxRange) > 2 * MeleeRange
  }

  override protected def wrapBase(unit: MobileRangeWeapon) = new SingleUnitBehaviour[MobileRangeWeapon](unit, meta) {
    private var attached       = false
    private var lastDecision   = ""
    private var shootDecidedAt = Option.empty[Int]
    private var lastCooldown   = 0

    /** The static defence this unit last danced with, so a mended flyer finds its way back to the fight. */
    private var lastTurret = Option.empty[(Point, Double)]

    /** Shots fired since this unit last stood outside static defence reach: a group pass ends after one volley. */
    private var shotsThisPass  = 0
    override def describeShort = "Ranged micro"

    override def onStealUnit(): Unit = {
      super.onStealUnit()
      lastTurret = None
    }

    override def toOrder(what: Objective) = {
      val me    = this.unit
      val frame = currentTick
      if (!attached) {
        attached = true
        NativeMatchEvidence.trace("kite-attached", s"unit=${me.nativeUnitId} type=${me.nativeUnit.getType}")
      }
      val targets       = targetsAround(me)
      val shooter       = shooterOf(me, targets.map(_._1))
      val passable      = if (me.nativeUnit.isFlying) insideMap else walkable
      val committed     = commitments(frame)
      val threatPoints  = targets.map(_._2)
      val inTankGroup   = me.isInstanceOf[Tank] && !me.nativeUnit.isSieged
      val meleeStandoff = if (inTankGroup) TankMeleeStandoff else ContactSlack
      val rally         = if (inTankGroup) tankRally(me, threatPoints) else None
      val targeted      = threatPoints.exists(_.aimsAtMe) || me.isBeingAttacked
      // hurt flyers with SCVs waiting cycle out to them as a group and come back mended
      val repairSupport = if (me.nativeUnit.isFlying) repairRallyOf(me) else None
      // a flyer under three quarters makes the SCV detour: one pass under two cannons costs about half its
      // health, so entering hurt is how tankers die mid-pass; scratches above that ride the next pass
      val hurt = (me.nativeUnit.getHitPoints + me.nativeUnit.getShields) * 4 <
        (me.nativeUnit.getType.maxHitPoints + me.nativeUnit.getType.maxShields) * 3
      val repairRally = if (hurt) repairSupport else None
      val danceNow    = staticDance(me, threatPoints, shooter)
      // against static defence alone, everybody burns the same target down instead of splitting over shields
      val modeNow = if (threatPoints.nonEmpty && threatPoints.forall(_.speed == 0)) FocusMode.Weakest else focusMode
      threatPoints.filter(t => t.speed == 0 && t.reach > 0).minByOption(t => shooter.at.distanceTo(t.at))
        .foreach(t => lastTurret = Some(t.at -> t.reach))
      val insideNow = threatPoints.exists(t =>
        t.speed == 0 && t.reach > 0 && shooter.at.distanceTo(t.at) - t.reach < StandOff
      )
      if (!insideNow) shotsThisPass = 0
      // group passes run on the shared pass rhythm: dive only in Diving, leave together in Exiting
      val passActive = danceNow == Dance.All && me.nativeUnit.isFlying
      val phase      = if (passActive) updatePass(frame, me, threatPoints) else PassPhase.Diving
      // one volley per pass: a second shot costs a full reload inside the reach for the same one-shot damage
      val leaveNow = passActive && phase == PassPhase.Exiting
      // menders do not hold the pass up: waiting for them let shields regrow faster than volleys damaged them
      val groupReady = !passActive || phase == PassPhase.Diving
      val decision   =
        decide(
          shooter,
          threatPoints,
          passable,
          lead = leadFrames,
          committed = committed.toMap,
          speedRatio = speedRatio,
          dance = danceNow,
          groundWalkable = terrainWalkable,
          mode = modeNow,
          meleeStandoff = meleeStandoff,
          fallBackWhenTargeted = inTankGroup && targeted,
          rally = rally,
          repairRally = repairRally,
          leaveNow = leaveNow,
          groupReady = groupReady
        )
      decision match {
        case Shoot(id) =>
          targets.find(_._2.id == id).foreach { case (enemy, _) =>
            val weapon = weaponAgainst(me.nativeUnit.getType, enemy.nativeUnit.isFlying)
            committed(id) = committed.getOrElse(id, 0.0) + weapon.damageAmount * weapon.damageFactor
          }
        case _ =>
      }

      // a shot resets the cooldown to the weapon's full reload
      val fired = shooter.cooldown > lastCooldown
      if (fired) {
        shotsThisPass += 1
        if (passPhase == PassPhase.Diving) firedThisPass += me.nativeUnitId
      }
      lastCooldown = shooter.cooldown
      shootDecidedAt.foreach { decidedAt =>
        if (fired) {
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
              s"gap=${gap.map(_.toInt).getOrElse(-1)} aimedAt=${targets.exists(_._2.aimsAtMe)} " +
              s"order=${me.nativeUnit.getOrder}"
          )
          lastDecision = kind
        }
      }
      decision match {
        // mended at the SCVs, beyond the range where the cannons count as threats: fly back to the standoff
        // outside their reach and rejoin the pass there — never alone into weapon range
        case Free if repairSupport.isDefined && !hurt && lastTurret.isDefined =>
          val (turret, reach) = lastTurret.get
          val scale           = (reach + StandOff) / shooter.at.distanceTo(turret)
          val to = Point(turret.x + (shooter.at.x - turret.x) * scale, turret.y + (shooter.at.y - turret.y) * scale)
          Orders.MoveToTile(me, MapTilePosition.shared(to.x.toInt / 32, to.y.toInt / 32)).toList
        case Free                        => Nil
        case Hold                        => Orders.NoUpdate(me).toList
        case Shoot(_) if shootByStopping => Orders.Stop(me).toList
        case Shoot(id)                   => targets.find(_._2.id == id).map(t => Orders.AttackUnit(me, t._1)).toList
        case Retreat(to)                 =>
          Orders.MoveToTile(me, MapTilePosition.shared(to.x.toInt / 32, to.y.toInt / 32)).toList
        case Approach(to) =>
          Orders.MoveToTile(me, MapTilePosition.shared(to.x.toInt / 32, to.y.toInt / 32)).toList
      }
    }
  }

  private def position(u: WrapsUnit) = Point(u.nativeUnit.getX.toDouble, u.nativeUnit.getY.toDouble)

  /**
    * The point a group of unsieged tanks falls back through together: their centre shifted one retreat step away
    * from the enemies they can see, so their individual retreats converge instead of fanning out.
    */
  private def tankRally(me: MobileRangeWeapon, threats: Seq[Threat]): Option[Point] = {
    val at     = position(me)
    val allies = ownUnits.allByType[Tank]
      .filter(t => t.isInGame && !t.isBeingCreated && position(t).distanceTo(at) <= ConsiderRadius)
      .toVector
    if (allies.size < 2 || threats.isEmpty) None
    else {
      val center =
        Point(allies.map(t => position(t).x).sum / allies.size, allies.map(t => position(t).y).sum / allies.size)
      val enemyCenter = Point(threats.map(_.at.x).sum / threats.size, threats.map(_.at.y).sum / threats.size)
      val (dx, dy)    = (center.x - enemyCenter.x, center.y - enemyCenter.y)
      val length      = math.hypot(dx, dy)
      if (length == 0) Some(center)
      else Some(Point(center.x + dx / length * 96, center.y + dy / length * 96))
    }
  }

  /**
    * The dance of a flyer against static defence alone, scaled by how badly the shooters around outgun it (KitingPolicy
    * .overwhelms): an overwhelming flight keeps firing while only the targeted unit steps out; anything less
    * goes in and out as a group. Anything but static defence keeps the configured dance.
    */
  private def staticDance(me: MobileRangeWeapon, threats: Seq[Threat], shooter: Shooter): Dance = {
    val turrets = threats.filter(t => t.speed == 0 && t.reach > 0)
    if (!me.nativeUnit.isFlying || turrets.isEmpty || !threats.forall(_.speed == 0)) dance
    else {
      val allies = ownUnits.allByType[MobileRangeWeapon]
        .count(a => a.exists && position(a).distanceTo(shooter.at) <= ConsiderRadius)
      val focus  = turrets.minBy(t => shooter.at.distanceTo(t.at))
      val weapon = weaponAgainst(me.nativeUnit.getType, targetFlies = false)
      val myDps  = focus.focus.shotDamage / math.max(1, weapon.damageCooldown)
      if (overwhelms(math.max(1, allies), myDps, turrets.map(_.focus.dps).sum, overwhelmRatio)) Dance.Aimed
      else Dance.All
    }
  }

  /**
    * Whether every flyer of this unit's flight is ready to go in together: weapon about ready, already shooting, or
    * out mending. A badly hurt flyer holds the pass up while repair waits for it (`mendingBlocks`), so the
    * flight goes in at full strength; without repair it is left behind. Ground shooters never wait.
    */
  private def flightReady(me: MobileRangeWeapon, threats: Seq[Threat], mendingBlocks: Boolean): Boolean = {
    val turrets = threats.filter(t => t.speed == 0 && t.reach > 0)
    if (!me.nativeUnit.isFlying || turrets.isEmpty) true
    else {
      val at = position(me)
      ownUnits.allByType[MobileRangeWeapon].forall { ally =>
        val n = ally.nativeUnit
        !ally.isInGame || ally.nativeUnitId == me.nativeUnitId || !n.isFlying ||
        position(ally).distanceTo(at) > ConsiderRadius ||
        n.getGroundWeaponCooldown <= leadFrames ||
        (!mendingBlocks && (n.getHitPoints + n.getShields) * 2 < n.getType.maxHitPoints + n.getType.maxShields) ||
        turrets.exists(t => position(ally).distanceTo(t.at) <= shooterOf(ally, Nil).range)
      }
    }
  }

  /** Where the SCVs that could mend this flyer wait: their centre, when any wait within reach of the fight. */
  private def repairRallyOf(me: MobileRangeWeapon): Option[Point] = {
    val at   = position(me)
    val scvs = ownUnits.allByType[SCV]
      .filter(s => s.isInGame && !s.isBeingCreated && position(s).distanceTo(at) <= 24 * 32)
      .toVector
    if (scvs.isEmpty) None
    else Some(Point(scvs.map(s => position(s).x).sum / scvs.size, scvs.map(s => position(s).y).sum / scvs.size))
  }

  private def radius(t: UnitType) = (t.dimensionLeft + t.dimensionRight + 1) / 2.0

  private def weaponAgainst(attacker: UnitType, targetFlies: Boolean) =
    if (targetFlies) attacker.airWeapon else attacker.groundWeapon

  /** The weapon range fits the enemies at hand: ground range when any ground enemy is near, otherwise air range. */
  private def shooterOf(me: MobileRangeWeapon, enemies: Seq[WrapsUnit]) = {
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
      player.topSpeed(kind),
      native.isFlying
    )
  }

  /**
    * Visible, detected enemy units and static defence within the considered radius that this unit can shoot, with how
    * far they reach it. Unfinished or unpowered static defence cannot fight back.
    */
  private def targetsAround(me: MobileRangeWeapon): Vector[(CanDie, Threat)] = {
    val at       = position(me)
    val myKind   = me.nativeUnit.getType
    val myRadius = radius(myKind)
    val iFly     = me.nativeUnit.isFlying
    val myId     = me.nativeUnitId
    (enemies.allByType[Mobile].iterator ++ enemies.allByType[ArmedBuilding].iterator: Iterator[CanDie]).filter { e =>
      val native = e.nativeUnit
      e.isInGame && native.isVisible && native.isDetected && position(e).distanceTo(at) <= ConsiderRadius &&
      weaponAgainst(myKind, native.isFlying) != WeaponType.None
    }.map { e =>
      val native = e.nativeUnit
      val kind   = native.getType
      val weapon = weaponAgainst(kind, iFly)
      val reach  =
        if (weapon == WeaponType.None || !native.isCompleted || !native.isPowered) 0.0
        else native.getPlayer.weaponMaxRange(weapon) + radius(kind) + myRadius
      def aims(u: bwapi.Unit) = u != null && u.getID == myId
      e -> Threat(
        native.getID,
        position(e),
        native.getHitPoints + native.getShields,
        reach,
        if (kind.isBuilding) 0.0 else native.getPlayer.topSpeed(kind),
        aims(native.getTarget) || aims(native.getOrderTarget),
        !native.isFlying && !kind.isBuilding,
        if (focusMode == FocusMode.Weakest) FocusFacts() else focusFacts(me.nativeUnit, native)
      )
    }.toVector
  }

  /** What one shot of `shooter` does to `enemy` and what `enemy` does to `shooter`, as the focus-fire modes weigh it. */
  private def focusFacts(shooter: bwapi.Unit, enemy: bwapi.Unit) = {
    val mine   = weaponAgainst(shooter.getType, enemy.isFlying)
    val theirs = weaponAgainst(enemy.getType, shooter.isFlying)
    // shields take a hit in full; hit points what the damage type lets through against the size, less armor
    def hit(weapon: WeaponType, by: bwapi.Player, on: bwapi.Unit, armor: Int) =
      math.max(0.5, by.damage(weapon) * sizeShare(weapon.damageType, on.getType.size) - armor) * weapon.damageFactor
    val onShields = math.max(
      0.5,
      shooter.getPlayer.damage(mine) - enemy.getPlayer.getUpgradeLevel(bwapi.UpgradeType.Protoss_Plasma_Shields)
    ) *
      mine.damageFactor
    val onHull     = hit(mine, shooter.getPlayer, enemy, enemy.getPlayer.armor(enemy.getType))
    val shots      = enemy.getShields / onShields + enemy.getHitPoints / onHull
    val shotDamage = if (enemy.getShields > 0) onShields else onHull
    val dps        =
      if (theirs == WeaponType.None) 0.0
      else hit(theirs, enemy.getPlayer, shooter, shooter.getPlayer.armor(shooter.getType)) /
        math.max(1, theirs.damageCooldown)
    val kind  = enemy.getType
    val value = kind.mineralPrice + kind.gasPrice
    // a caster with energy for its spell is worth killing first, one without much less; cloakers hide others
    val worth = kind match {
      case bwapi.UnitType.Protoss_High_Templar => if (enemy.getEnergy >= 75) value + 300.0 else value * 0.2
      case bwapi.UnitType.Protoss_Arbiter      => value + 200.0
      case bwapi.UnitType.Zerg_Defiler | bwapi.UnitType.Terran_Science_Vessel | bwapi.UnitType.Zerg_Queen =>
        if (enemy.getEnergy >= 75) value + 200.0 else value * 0.5
      case bwapi.UnitType.Protoss_Dark_Templar | bwapi.UnitType.Terran_Ghost => value + 100.0
      case _                                                                 => value.toDouble
    }
    FocusFacts(shots, shotDamage, dps, worth)
  }

  private def sizeShare(damage: bwapi.DamageType, size: bwapi.UnitSizeType) = (damage, size) match {
    case (bwapi.DamageType.Concussive, bwapi.UnitSizeType.Medium) => 0.5
    case (bwapi.DamageType.Concussive, bwapi.UnitSizeType.Large)  => 0.25
    case (bwapi.DamageType.Explosive, bwapi.UnitSizeType.Small)   => 0.5
    case (bwapi.DamageType.Explosive, bwapi.UnitSizeType.Medium)  => 0.75
    case _                                                        => 1.0
  }

  private def insideMap(p: Point) = {
    val grid = mapLayers.rawWalkableMap
    p.x >= 0 && p.y >= 0 && p.x < grid.cols * 32 && p.y < grid.rows * 32
  }

  /** Terrain a ground unit could stand on, whatever stands there now. */
  private def terrainWalkable(p: Point) =
    p.x >= 0 && p.y >= 0 &&
      mapLayers.rawWalkableMap.containsAndFree(MapTilePosition.shared(p.x.toInt / 32, p.y.toInt / 32))

  private def walkable(p: Point) = {
    p.x >= 0 && p.y >= 0 && mapLayers.freeWalkableTiles.containsAndFree(MapTilePosition.shared(
      p.x.toInt / 32,
      p.y.toInt / 32
    ))
  }
}
