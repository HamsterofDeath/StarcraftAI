package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * Hit and run with Battlecruisers. They gather at a berth in the main until enough are fit, fly together to the
  * nearest known enemy base (expansions before the main), turn home one by one when hurt and all together when the
  * raid is worn down, and are mended at the berth by a crew of SCVs. A raid on our own bases big enough to recall the
  * army calls them home too.
  */
class CruiserRaids(universe: Universe) extends DefaultBehaviour[Battlecruiser](universe) {
  import CruiserTactics._

  private val crew      = new Employer[SCV](universe)
  private val repairing = mutable.HashSet.empty[Int]
  private var raiders   = Set.empty[Int]
  private var target    = Option.empty[MapTilePosition]
  private val swept     = mutable.HashSet.empty[MapTilePosition]
  private var myBerth   = Option.empty[MapTilePosition]

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
    repairing.filterInPlace(health.contains)
    health.foreach { (id, hp) =>
      val hurt = needsRepair(hp, repairing(id))
      if (hurt && !repairing(id)) {
        repairing += id
        NativeMatchEvidence.trace("cruiser-repair", s"cruiser=$id health=${(hp * 100).round}%")
      } else if (!hurt) repairing -= id
    }
    raiders = raiders.filter(id => health.contains(id) && !repairing(id))
    updateTarget(fleet)
    if (raiders.nonEmpty) {
      val worn = endsRaid(raiders.toSeq.map(health))
      if (worn || target.isEmpty || worldDominationPlan.recallsArmy) {
        NativeMatchEvidence.trace(
          "raid-end",
          s"raiders=${raiders.size} worn=$worn target=$target recall=${worldDominationPlan.recallsArmy}"
        )
        raiders.filter(id => health(id) < FitFrom).foreach(repairing += _)
        raiders = Set.empty
      }
    } else if (!worldDominationPlan.recallsArmy) {
      val fit = health.collect { case (id, hp) if !repairing(id) && hp >= FitFrom => id }.toSet
      if (startsRaid(fit.size, health.size) && target.isDefined) {
        raiders = fit
        NativeMatchEvidence.trace("raid-start", s"raiders=${fit.size} target=${target.get} fleet=${fleet.size}")
      }
    }
    worldDominationPlan.raidingFleet = fleet.filter(c => raiders(c.nativeUnitId)).toSet
    hireCrew(fleet.size)
  }

  /** Keeps the raid's target while enemy buildings stand there; a start location found empty is swept off the list. */
  private def updateTarget(fleet: collection.Set[Battlecruiser]): Unit = {
    val known = universe.pluginByType[RunTerranCampaign].enemyBuildings
    target.foreach { t =>
      val standing = known.exists(_.tile.distanceToIsLess(t, 10))
      val seen     = fleet.exists(c => raiders(c.nativeUnitId) && c.currentTile.distanceToIsLess(t, 5))
      if (!standing && seen) swept += t
      if (!standing && (seen || known.nonEmpty)) target = None
    }
    if (target.isEmpty) bases.mainBase.foreach { main =>
      val own    = nativeGame.self().getStartLocation
      val starts = nativeGame.getStartLocations.asScala.toVector.filterNot(_ == own)
        .map(t => MapTilePosition(t.x, t.y)).filterNot(swept)
      val ours = MapTilePosition(own.x, own.y)
      // resource areas nearer to an enemy start than to ours, the start itself aside: where expansions are likely
      val likely = strategicMap.resources.toVector.map(_.center).filter { c =>
        starts.exists(s => c.distanceSquaredTo(s) < c.distanceSquaredTo(ours)) &&
        !starts.exists(_.distanceToIsLess(c, 8))
      }.filterNot(swept)
      target = choose(
        known.filter(_.base).map(_.tile),
        likely,
        known.map(_.tile),
        starts,
        main.mainBuilding.tilePosition
      )
    }
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
      else if (repairing(id)) berth.filter(me.currentTile.distanceToIsMore(_, 2)).map(Orders.MoveToTile(me, _)).toList
      else if (raiders(id)) target.map(Orders.AttackMove(me, _)).toList
      else if (worldDominationPlan.baseDefenseActive) Nil
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

  /** At least RaidSize fit cruisers and two thirds of the fleet: the hurt are mended before the fleet sets out. */
  def startsRaid(fit: Int, fleet: Int) = fit >= RaidSize && fit * 3 >= fleet * 2

  def endsRaid(health: Seq[Double]) = health.size < 2 || health.sum / health.size < WornBelow

  /** Two to six SCVs, one more for every three cruisers. */
  def crewSize(cruisers: Int) = if (cruisers == 0) 0 else (2 + cruisers / 3) min 6

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
