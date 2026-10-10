package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * Hit and run with Battlecruisers. They wait at a berth in the main until enough are fit, gather, fly together to the
  * nearest known enemy base (expansions before the main) with the ones ahead waiting for the rest, and leave again as
  * soon as the enemy's anti-air near them outweighs them: hit, run, come back. A hurt cruiser turns home alone, the
  * whole raid when it is worn down, and a crew of SCVs mends them at the berth. Once the fleet is big it attacks as one
  * group that only turns back from a clearly stronger defence. A raid on our own bases big enough to recall the army
  * calls them home too.
  */
class CruiserRaids(universe: Universe) extends DefaultBehaviour[Battlecruiser](universe) {
  import CruiserTactics._

  private val crew       = new Employer[SCV](universe)
  private val repairing  = mutable.HashSet.empty[Int]
  private var raiders    = Set.empty[Int]
  private var target     = Option.empty[MapTilePosition]
  private val swept      = mutable.HashSet.empty[MapTilePosition]
  private var myBerth    = Option.empty[MapTilePosition]
  private var gathering  = false
  private var gatherAt   = 0
  private var bigGroup   = false
  private var centre     = Option.empty[MapTilePosition]
  private var lastStatus = -1

  private def active = race.isTerran && strategy.current.raidsWithCruisers

  /** Where hurt cruisers wait for the crew: in the main, a few tiles from the command center away from its minerals. */
  private def berth = {
    if (myBerth.isEmpty) myBerth = bases.mainBase.map { main =>
      val cc        = main.mainBuilding.centerTile
      val (dx, dy)  = main.resourceArea.map(ra => (cc.x - ra.center.x, cc.y - ra.center.y)).getOrElse((0, 0))
      val length    = math.max(1.0, math.hypot(dx, dy))
      val preferred = MapTilePosition(
        cc.x + (dx * BerthDistance / length).round.toInt,
        cc.y + (dy * BerthDistance / length).round.toInt
      )
      mapLayers.freeWalkableTiles.nearestFree(preferred).getOrElse(cc)
    }
    myBerth
  }

  private def cruisers = ownUnits.allByType[Battlecruiser].filter(c => c.isInGame && !c.isBeingCreated)

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (!active) {
      worldDominationPlan.raidingFleet = Set.empty
      return
    }
    val fleet  = cruisers
    val health = fleet.map(c => c.nativeUnitId -> c.percentageHPOk).toMap
    // without a crew nobody mends the cruisers: waiting for repairs would keep the fleet home for good
    val canMend = unitManager.allJobsByType[RepairCrewDuty].exists(j => !j.failedOrObsolete && !j.isFinished)
    repairing.filterInPlace(health.contains)
    if (!canMend) repairing.clear()
    else health.foreach { (id, hp) =>
      val hurt = needsRepair(hp, repairing(id))
      if (hurt && !repairing(id)) {
        repairing += id
        NativeMatchEvidence.trace("cruiser-repair", s"cruiser=$id health=${(hp * 100).round}%")
      } else if (!hurt) repairing -= id
    }
    raiders = raiders.filter(id => health.contains(id) && !repairing(id))
    updateTarget(fleet)
    val members = fleet.filter(c => raiders(c.nativeUnitId)).toVector
    centre = MapTilePosition.averageOpt(members.iterator.map(_.currentTile))
    if (
      gathering && centre.forall(c => gathered(members.map(_.currentTile), c) || currentTick - gatherAt > GatherFrames)
    ) {
      gathering = false
      NativeMatchEvidence.trace("raid-gathered", s"raiders=${members.size} at=$centre frames=${currentTick - gatherAt}")
    }
    if (raiders.nonEmpty) {
      val worn       = if (canMend) endsRaid(raiders.toSeq.map(health)) else raiders.size < 2
      val antiAir    = centre.map(antiAirAround).getOrElse(0)
      val strength   = members.map(m => CruiserValue * health(m.nativeUnitId)).sum
      val overpowerd = !gathering && outnumbered(antiAir, strength, bigGroup)
      if (worn || overpowerd || target.isEmpty || worldDominationPlan.recallsArmy) {
        NativeMatchEvidence.trace(
          "raid-end",
          s"raiders=${raiders.size} worn=$worn outnumbered=$overpowerd antiAir=$antiAir strength=${strength.round} " +
            s"group=$bigGroup target=$target recall=${worldDominationPlan.recallsArmy}"
        )
        raiders.filter(id => health(id) < FitFrom).foreach(repairing += _)
        raiders = Set.empty
      }
    } else if (!worldDominationPlan.recallsArmy) {
      val fit = health.collect { case (id, hp) if !repairing(id) && (hp >= FitFrom || !canMend) => id }.toSet
      if (startsRaid(fit.size, health.size) && target.isDefined) {
        raiders = fit
        bigGroup = health.size >= BigFleet
        gathering = true
        gatherAt = currentTick
        NativeMatchEvidence.trace(
          "raid-start",
          s"raiders=${fit.size} target=${target.get} fleet=${fleet.size} group=$bigGroup"
        )
      }
    }
    worldDominationPlan.raidingFleet = fleet.filter(c => raiders(c.nativeUnitId)).toSet
    hireCrew(fleet.size)
    if (fleet.nonEmpty && currentTick / 720 != lastStatus) {
      lastStatus = currentTick / 720
      val fitNow = health.count((id, hp) => !repairing(id) && (hp >= FitFrom || !canMend))
      NativeMatchEvidence.trace(
        "raid-status",
        s"fleet=${fleet.size} fit=$fitNow repairing=${repairing.size} raiders=${raiders.size} gathering=$gathering " +
          s"target=$target canMend=$canMend pressure=${worldDominationPlan.baseDefenseActive} " +
          s"recall=${worldDominationPlan.recallsArmy} atBerth=${fleet.count(c =>
              berth.exists(b => !c.currentTile.distanceToIsMore(b, 8))
            )} " + s"hurt=$hurtDetail crew=$crewDetail"
      )
    }
  }

  /** The hurt as the game reports them, the bot's cached health in brackets, and their orders. */
  private def hurtDetail = cruisers.filter(c => repairing(c.nativeUnitId)).take(6).map { c =>
    s"${c.nativeUnitId}:${c.nativeUnit.getHitPoints}[${(c.percentageHPOk * 100).round}]:${c.nativeUnit.getOrder}"
  }.mkString(",")

  /** What the crew is busy with: each order and its target's type, counted. */
  private def crewDetail = unitManager.allJobsByType[RepairCrewDuty].filter(j => !j.failedOrObsolete).map { j =>
    val w = j.unit.nativeUnit
    s"${w.getOrder}>${Option(w.getOrderTarget).map(_.getType.toString.stripPrefix("Terran_")).getOrElse("-")}"
  }.groupBy(identity).map((k, v) => s"$k*${v.size}").mkString(",")

  /** Keeps the raid's target while enemy buildings stand there; a start location found empty is swept off the list. */
  private def updateTarget(fleet: collection.Set[Battlecruiser]): Unit = {
    val known = universe.pluginByType[RunTerranCampaign].enemyBuildings
    target.foreach { t =>
      val standing = known.exists(_.tile.distanceToIsLess(t, 10))
      val seen     = fleet.exists(c => raiders(c.nativeUnitId) && c.currentTile.distanceToIsLess(t, 5))
      if (!standing && seen) swept += t
      if (!standing && (seen || known.nonEmpty)) target = None
    }
    if (target.isEmpty) {
      val own       = nativeGame.self().getStartLocation
      val ours      = MapTilePosition(own.x, own.y)
      val home      = bases.mainBase.map(_.mainBuilding.tilePosition).getOrElse(ours)
      val allStarts =
        nativeGame.getStartLocations.asScala.toVector.filterNot(_ == own).map(t => MapTilePosition(t.x, t.y))
      val starts = allStarts.filterNot(swept)
      // every resource area without a base of ours: the hunt's last resort when the enemy hides
      val fields = strategicMap.resources.toVector.map(_.center)
        .filterNot(c => bases.allBases.exists(_.mainBuilding.tilePosition.distanceToIsLess(c, 10)))
      // resource areas nearer to an enemy start than to ours, the start itself aside: where expansions are likely
      val likely = fields.filter { c =>
        allStarts.exists(s => c.distanceSquaredTo(s) < c.distanceSquaredTo(ours)) &&
        !allStarts.exists(_.distanceToIsLess(c, 8))
      }.filterNot(swept)
      def pick = choose(known.filter(_.base).map(_.tile), likely, known.map(_.tile), starts, home)
        .orElse(fields.filterNot(swept).minByOpt(_.distanceSquaredTo(home)))
      // all swept and still no enemy found: sweep again, the enemy may have built somewhere since
      target = pick.orElse {
        if (swept.isEmpty) None
        else {
          swept.clear()
          pick
        }
      }
    }
  }

  /** Minerals and gas of the visible enemies within sight of the raid that can shoot at air units. */
  private def antiAirAround(at: MapTilePosition) = {
    val near = (enemies.allByType[Mobile].iterator ++ enemies.allByType[ArmedBuilding].iterator: Iterator[WrapsUnit])
      .filter(e => e.isInGame && e.nativeUnit.isVisible && e.currentTile.distanceToIsLess(at, RaidSight))
    near.map(_.nativeUnit.getType).filter(t =>
      t.airWeapon != bwapi.WeaponType.None || t == bwapi.UnitType.Protoss_Carrier
    )
      .map(t => t.mineralPrice + t.gasPrice).sum
  }

  private def hireCrew(cruiserCount: Int): Unit = berth.foreach { home =>
    val wanted   = crewSize(cruiserCount)
    val employed = unitManager.allJobsByType[RepairCrewDuty].count(j => !j.failedOrObsolete && !j.isFinished)
    if (employed < wanted) {
      val request = UnitJobRequest.idleOfType(crew, classOf[SCV], wanted - employed, Priority.Supply)
        .withOnlyAccepting { w =>
          val job = unitManager.jobOf(w)
          w.currentArea.exists(_.free(home)) && !ferryManager.sealedApart(w.currentTile, home) &&
          (job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch])
        }.withRequest(_.withCherryPicker_!(UnitRequest.CherryPickers.cherryPickWorkerByDistance[SCV](home)()))
      unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
        crew.assignJob_!(new RepairCrewDuty(w, home, crew))
        NativeMatchEvidence.trace("cruiser-crew", s"scv=${w.nativeUnitId} berth=$home cruisers=$cruiserCount")
      }
    }
  }

  override protected def wrapBase(unit: Battlecruiser) = new SingleUnitBehaviour[Battlecruiser](unit, meta) {
    override def describeShort = "Cruiser raid"

    // a hurt cruiser leaves the fight: above the ranged micro that would keep it shooting
    override def priority = if (repairing(this.unit.nativeUnitId)) RetreatPriority else super.priority

    override protected def toOrder(what: Objective) = {
      val me = this.unit
      val id = me.nativeUnitId
      if (!active || me.isBeingCreated) Nil
      // at the berth a hurt cruiser holds still for the crew: one that drifts off after other orders is never mended
      else if (repairing(id)) berth.map { b =>
        if (me.currentTile.distanceToIsMore(b, 2)) Orders.MoveToTile(me, b) else Orders.HoldPosition(me)
      }.toList
      else if (raiders(id)) {
        // gathering, or ahead of the others: meet them first, so the raid arrives as one
        val meet = centre.filter(c => gathering || target.exists(t => waitsForGroup(me.currentTile, c, t)))
        meet.orElse(target).map(Orders.AttackMove(me, _)).toList
      } else if (worldDominationPlan.baseDefenseActive) Nil
      else berth.filter(me.currentTile.distanceToIsMore(_, 8)).map(Orders.AttackMove(me, _)).toList
    }
  }
}

/** The fleet decisions of CruiserRaids, kept free of the game. */
private[pony] object CruiserTactics {

  /** Below this share of its hit points a cruiser flies home for repair... */
  val RetreatBelow = 0.4

  /** ...and from this share on it is fit to raid again. */
  val FitFrom = 0.9

  /** A raid starts with this many fit cruisers... */
  val RaidSize = 3

  /** ...and ends when fewer than two stay on it or their mean health falls below this share. */
  val WornBelow = 0.55

  val BerthDistance = 6

  val RetreatPriority = SecondPriority(0.95)

  def needsRepair(health: Double, repairing: Boolean) = health < (if (repairing) FitFrom else RetreatBelow)

  /**
    * At least RaidSize fit cruisers and two thirds of the fleet: the hurt are mended before the fleet sets out. A big
    * fleet attacks as one group, so it waits for four fifths.
    */
  def startsRaid(fit: Int, fleet: Int) =
    fit >= RaidSize && (if (fleet >= BigFleet) fit * 5 >= fleet * 4 else fit * 3 >= fleet * 2)

  /** From this many cruisers on the fleet attacks as one group. */
  val BigFleet = 10

  /** Minerals and gas of one cruiser, the measure of a raid's strength (times its health). */
  val CruiserValue = 700

  /** Tiles around the raid's centre in which enemy anti-air counts against it. */
  val RaidSight = 12

  /** A raid hits and runs: it leaves once the anti-air near it outweighs it by this much; a big group stays longer. */
  val RunRatio      = 0.8
  val GroupRunRatio = 1.5

  def outnumbered(antiAir: Int, strength: Double, bigGroup: Boolean) =
    antiAir > strength * (if (bigGroup) GroupRunRatio else RunRatio)

  /** Gathered once every raider is this close to the centre, or after GatherFrames at the latest. */
  val GatherRadius = 6
  val GatherFrames = 24 * 30

  def gathered(raiders: Seq[MapTilePosition], centre: MapTilePosition) =
    raiders.forall(r => !r.distanceToIsMore(centre, GatherRadius))

  /** A raider more than StrayRadius tiles from the centre, on the target's side of it, waits for the others. */
  val StrayRadius = 8

  def waitsForGroup(me: MapTilePosition, centre: MapTilePosition, target: MapTilePosition) =
    me.distanceToIsMore(centre, StrayRadius) && me.distanceSquaredTo(target) < centre.distanceSquaredTo(target)

  def endsRaid(health: Seq[Double]) = health.size < 2 || health.sum / health.size < WornBelow

  /** Two to ten SCVs, one more for every two cruisers. */
  def crewSize(cruisers: Int) = if (cruisers == 0) 0 else (2 + cruisers / 2) min 10

  /**
    * The nearest known enemy base that is no start location (an expansion is defended least), else the nearest
    * unvisited site where the enemy likely expanded, else the nearest base, else the nearest enemy building, else the
    * nearest enemy start not yet found empty.
    */
  def choose(
      enemyBases: Seq[MapTilePosition],
      likelyExpansions: Seq[MapTilePosition],
      enemyBuildings: Seq[MapTilePosition],
      enemyStarts: Seq[MapTilePosition],
      home: MapTilePosition
  ): Option[MapTilePosition] = {
    def nearest(tiles: Seq[MapTilePosition]) = tiles.minByOpt(_.distanceSquaredTo(home))
    val expansions                           = enemyBases.filterNot(b => enemyStarts.exists(_.distanceToIsLess(b, 8)))
    nearest(expansions).orElse(nearest(likelyExpansions)).orElse(nearest(enemyBases))
      .orElse(nearest(enemyBuildings)).orElse(nearest(enemyStarts))
  }
}

/** One SCV of the cruisers' repair crew: it waits at the berth and mends the most hurt cruiser hovering there. */
private[pony] class RepairCrewDuty(worker: SCV, berth: MapTilePosition, owner: Employer[SCV])
    extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  override def shortDebugString         = "Cruiser repair crew"
  override def isFinished               = false
  override def jobHasFailedWithoutDeath = false
  override def everyNth                 = 23

  override def ordersForTick = {
    val hurt = ownUnits.allByType[Battlecruiser].filter { c =>
      c.isInGame && !c.isBeingCreated && c.isDamaged && c.currentTile.distanceToIsLess(berth, 8)
    }
    hurt.minByOpt(_.percentageHPOk).map(c => Orders.RepairUnit(worker, c)).orElse {
      Option.when(worker.currentTile.distanceToIsMore(berth, 3))(Orders.MoveToTile(worker, berth))
    }.toSeq
  }
}
