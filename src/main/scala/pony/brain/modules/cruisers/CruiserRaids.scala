package pony
package brain
package modules
package cruisers

import pony.brain.jobs.Employer
import pony.brain.modules.campaign.{EnemyArmyEstimate, RunTerranCampaign}
import pony.brain.modules.economy.GatherMineralsAtSinglePatch
import pony.brain.requests.{UnitJobRequest, UnitRequest}
import pony.combat.ArmedBuilding
import pony.geometry.MapTilePosition
import pony.units.{Battlecruiser, Mobile, SCV, WorkerUnit, WrapsUnit}

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

  private val crew         = new Employer[SCV](universe)
  private val repairing    = mutable.HashSet.empty[Int]
  private var raiders      = Set.empty[Int]
  private var target       = Option.empty[MapTilePosition]
  private val swept        = mutable.HashSet.empty[MapTilePosition]
  private var myBerth      = Option.empty[MapTilePosition]
  private var gathering    = false
  private var gatherAt     = 0
  private var gatheredAt   = 0
  private var closest      = Double.MaxValue
  private var progressAt   = 0
  private var bigGroup     = false
  private var centre       = Option.empty[MapTilePosition]
  private var lastStatus   = -1
  private var starvedSince = Option.empty[Int]
  private val trail        = mutable.Queue.empty[(Int, MapTilePosition)]
  // the most anti-air a raid met at each target, and when: the next raid there must be strong enough for it
  private val defended    = mutable.HashMap.empty[MapTilePosition, (Int, Int)]
  private var peakAntiAir = 0
  // since when the raid has stood at its target without destroying anything, and how many enemies were destroyed then
  private var lingerSince  = Option.empty[Int]
  private var destroyedNow = 0
  private var fieldSpot    = Option.empty[MapTilePosition]
  private val fieldCrew    = new Employer[SCV](universe)

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
    // Without a crew nobody mends the cruisers, and repairs cost minerals and gas: a bank empty of either for two
    // minutes stops them too. Waiting for repairs that never come would keep the fleet home for good.
    val player = nativeGame.self()
    if (player.minerals >= 50 && player.gas >= 50) starvedSince = None
    else if (starvedSince.isEmpty) starvedSince = Some(currentTick)
    val canMend = unitManager.allJobsByType[RepairCrewDuty].exists(j => !j.failedOrObsolete && !j.isFinished) &&
      starvedSince.forall(currentTick - _ < StarvedFrames)
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
      gatheredAt = currentTick
      NativeMatchEvidence.trace("raid-gathered", s"raiders=${members.size} at=$centre frames=${currentTick - gatherAt}")
    }
    // a raid that comes no nearer its target for two minutes, with nobody there, gives the target up
    for (c <- centre; t <- target if raiders.nonEmpty && !gathering) {
      val distance = math.sqrt(c.distanceSquaredTo(t).toDouble)
      if (distance + ProgressTiles <= closest) {
        closest = distance
        progressAt = currentTick
      } else if (currentTick - progressAt > StallFrames && !members.exists(m => m.currentTile.distanceToIsLess(t, 6))) {
        NativeMatchEvidence.trace("raid-stuck", s"target=$t centre=$c raiders=${members.size}")
        swept += t
        target = None
        closest = Double.MaxValue
        progressAt = currentTick
        updateTarget(fleet)
      }
    }
    // a raid at its target that destroys nothing for a while gives the target up: in the watched game on 4acedd3 the
    // raid hovered at one spot from minute 48 on, the game long won, while a building it never reached kept it there
    val destroyed = world.observedDestroyedEnemies.size
    if (destroyed != destroyedNow) { destroyedNow = destroyed; lingerSince = None }
    val atTarget = raiders.nonEmpty && !gathering &&
      centre.zip(target).exists((c, t) => c.distanceToIsLess(t, LingerTiles))
    if (!atTarget) lingerSince = None
    else if (lingerSince.isEmpty) lingerSince = Some(currentTick)
    for (since <- lingerSince; t <- target if currentTick - since > LingerFrames) {
      NativeMatchEvidence.trace("raid-lingering", s"target=$t raiders=${raiders.size} frames=${currentTick - since}")
      swept += t
      target = None
      lingerSince = None
      updateTarget(fleet)
    }
    if (raiders.nonEmpty) {
      val worn       = if (canMend) endsRaid(raiders.toSeq.map(health)) else raiders.size < 2
      val antiAir    = centre.map(antiAirAround).getOrElse(0)
      val strength   = members.map(m => CruiserValue * health(m.nativeUnitId)).sum
      val overpowerd = !gathering && outnumbered(antiAir, strength, bigGroup)
      if (!gathering) peakAntiAir = peakAntiAir max antiAir
      if (worn || overpowerd || target.isEmpty || worldDominationPlan.recallsArmy) {
        target.foreach(t => defended(t) = (peakAntiAir, currentTick))
        NativeMatchEvidence.trace(
          "raid-end",
          s"raiders=${raiders.size} worn=$worn outnumbered=$overpowerd antiAir=$antiAir strength=${strength.round} " +
            s"group=$bigGroup target=$target recall=${worldDominationPlan.recallsArmy}"
        )
        raiders.filter(id => health(id) < FitFrom).foreach(repairing += _)
        raiders = Set.empty
      }
    } else if (!worldDominationPlan.recallsArmy) {
      val fit     = health.collect { case (id, hp) if !repairing(id) && (hp >= FitFrom || !canMend) => id }.toSet
      val group   = health.size >= BigFleet
      val defence = target.map(defenceAt).getOrElse(0)
      if (startsRaid(fit.size, health.size) && target.isDefined && strongEnough(fit.size, defence, group)) {
        raiders = fit
        bigGroup = group
        peakAntiAir = 0
        gathering = true
        gatherAt = currentTick
        closest = Double.MaxValue
        progressAt = currentTick
        NativeMatchEvidence.trace(
          "raid-start",
          s"raiders=${fit.size} target=${target.get} fleet=${fleet.size} group=$bigGroup " +
            s"strength=${fit.size * CruiserValue} defence=$defence " +
            s"enemyAtMost=${universe.pluginByType[EnemyArmyEstimate].maxArmyAt(target.get).round}"
        )
      }
    }
    updateFieldCrew(health)
    worldDominationPlan.raidingFleet = fleet.filter(c => raiders(c.nativeUnitId)).toSet
    templarNear = raiders.nonEmpty && {
      val raiding = fleet.filter(c => raiders(c.nativeUnitId)).map(_.currentTile)
      enemies.allByType[Mobile].exists(e =>
        e.isInGame && e.nativeUnit.isVisible && e.nativeUnit.getType == bwapi.UnitType.Protoss_High_Templar &&
          raiding.exists(_.distanceToIsLess(e.currentTile, TemplarSight))
      )
    }
    worldDominationPlan.cruisersInRepair = repairing.size
    worldDominationPlan.cruiserRaidCentre = if (raiders.nonEmpty) centre else None
    worldDominationPlan.cruiserBerth = berth
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

  /**
    * With a High Templar in sight of the raid the cruisers spread, a storm catching one at most; otherwise they stack.
    * A raider closer than `StackSpacing` to another steps away from them.
    */
  private def spaceOut(me: Battlecruiser): Option[MapTilePosition] =
    if (!templarNear || !raiders(me.nativeUnitId)) None
    else {
      val others = cruisers.filter(c => raiders(c.nativeUnitId) && c.nativeUnitId != me.nativeUnitId)
        .map(_.currentTile).filter(t => !t.distanceToIsMore(me.currentTile, SpreadTiles - 1))
      if (others.isEmpty) None
      else {
        val (ox, oy) = (others.map(_.x).sum.toDouble / others.size, others.map(_.y).sum.toDouble / others.size)
        val (dx, dy) = (me.currentTile.x - ox, me.currentTile.y - oy)
        val length   = math.hypot(dx, dy)
        val (ux, uy) = if (length < 0.1) (1.0, 0.0) else (dx / length, dy / length)
        val grid     = mapLayers.rawWalkableMap
        Some(MapTilePosition.shared(
          (me.currentTile.x + ux * SpreadTiles).round.toInt.max(0).min(grid.cols - 1),
          (me.currentTile.y + uy * SpreadTiles).round.toInt.max(0).min(grid.rows - 1)
        ))
      }
    }

  private var templarNear = false

  /** The hurt as the game reports them, the bot's cached health in brackets, and their orders. */
  private def hurtDetail = cruisers.filter(c => repairing(c.nativeUnitId)).take(6).map { c =>
    s"${c.nativeUnitId}:${c.nativeUnit.getHitPoints}[${(c.percentageHPOk * 100).round}]:${c.nativeUnit.getOrder}" +
      s":${unitManager.jobOf(c).getClass.getSimpleName}"
  }.mkString(",")

  /** What the crew is busy with: each order and its target's type, counted. */
  private def crewDetail = unitManager.allJobsByType[RepairCrewDuty].filter(j => !j.failedOrObsolete).map { j =>
    val w = j.unit.nativeUnit
    s"${w.getOrder}>${Option(w.getOrderTarget).map(_.getType.toString.stripPrefix("Terran_")).getOrElse("-")}"
  }.groupBy(identity).map((k, v) => s"$k*${v.size}").mkString(",")

  /** The anti-air the last raid at this target met, while that is recent enough to count. */
  private def defenceAt(t: MapTilePosition) = remembered(defended.get(t), currentTick)

  /** Keeps the raid's target while enemy buildings stand there; a start location found empty is swept off the list. */
  private def updateTarget(fleet: collection.Set[Battlecruiser]): Unit = {
    val known = universe.pluginByType[RunTerranCampaign].enemyBuildings
    target.foreach { t =>
      val standing = known.exists(_.tile.distanceToIsLess(t, 10))
      val seen     = fleet.exists(c => raiders(c.nativeUnitId) && c.currentTile.distanceToIsLess(t, 5))
      if (!standing && seen) swept += t
      if (!standing && (seen || known.nonEmpty)) target = None
    }
    // a target with buildings near it moves onto the nearest of them: an attack-move ends at its point, and a building
    // nine tiles off stays out of the cruisers' reach
    target = target.map { t =>
      if (known.exists(_.tile == t)) t
      else known.filter(_.tile.distanceToIsLess(t, 10)).minByOpt(_.tile.distanceSquaredTo(t)).map(_.tile).getOrElse(t)
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
      val estimate = universe.pluginByType[EnemyArmyEstimate]
      // buildings at a target given up for lingering are left alone until everything has been swept
      val left = known.filterNot(b => swept.exists(_.distanceToIsLess(b.tile, 10)))
      def pick = choose(
        left.filter(_.base).map(_.tile),
        likely,
        left.map(_.tile),
        starts,
        home,
        t => estimate.armyNear(t) + defenceAt(t)
      )
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

  /**
    * A raid holding still away from enemy ground fighters, with enough cruisers hurt, calls a field crew to the spot
    * under it; the spot stays while the raid holds still, and the crew goes home when it is gone.
    */
  private def updateFieldCrew(health: Map[Int, Double]): Unit = {
    centre.filter(_ => raiders.nonEmpty && !gathering) match {
      case Some(c) =>
        trail += currentTick -> c
        while (trail.headOption.exists(_._1 < currentTick - 2 * FieldRepair.StationaryFrames)) trail.dequeue()
      case None => trail.clear()
    }
    val enemyGround = centre.exists(c =>
      unitGrid.enemy.allInRange[Mobile](c, FieldRepair.SafeTiles).exists(e =>
        !e.nativeUnit.isFlying && !e.isHarmlessNow && !e.isInstanceOf[WorkerUnit]
      )
    )
    val holding = raiders.nonEmpty && FieldRepair.stationary(trail.toSeq, currentTick) && !enemyGround
    val hurt    = raiders.count(id => health.get(id).exists(_ < FieldRepair.HurtBelow))
    val before  = fieldSpot
    fieldSpot =
      if (!holding) None
      else fieldSpot.orElse {
        if (FieldRepair.wanted(stationary = true, hurt, enemyGround))
          centre.flatMap(mapLayers.freeWalkableTiles.nearestFree)
        else None
      }
    if (before != fieldSpot)
      NativeMatchEvidence.trace("field-spot", s"spot=$fieldSpot hurt=$hurt raiders=${raiders.size}")
    for (spot <- fieldSpot; home <- berth) {
      val employed = unitManager.allJobsByType[FieldCrewDuty].count(j => !j.failedOrObsolete && !j.isFinished)
      if (employed < FieldRepair.CrewSize) {
        val request =
          UnitJobRequest.idleOfType(fieldCrew, classOf[SCV], FieldRepair.CrewSize - employed, Priority.Supply)
            .withOnlyAccepting { w =>
              val job = unitManager.jobOf(w)
              w.onGround && (job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch])
            }.withRequest(_.withCherryPicker_!(UnitRequest.CherryPickers.cherryPickWorkerByDistance[SCV](spot)()))
        unitManager.request(request, buildIfNoneAvailable = false).units.foreach { w =>
          fieldCrew.assignJob_!(new FieldCrewDuty(w, () => fieldSpot, home, fieldCrew))
          NativeMatchEvidence.trace("field-crew", s"scv=${w.nativeUnitId} spot=$spot")
        }
      }
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

    // a hurt cruiser leaves the fight, and one crowding its neighbours under storm threat steps aside: both above the
    // ranged micro that would keep it shooting
    override def priority =
      if (repairing(this.unit.nativeUnitId)) RetreatPriority
      else if (spaceOut(this.unit).isDefined) SpacingPriority
      else super.priority

    override protected def toOrder(what: Objective) = {
      val me = this.unit
      val id = me.nativeUnitId
      if (!active || me.isBeingCreated) Nil
      // at the berth a hurt cruiser holds still for the crew: one that drifts off after other orders is never mended
      else if (repairing(id)) berth.map { b =>
        // right over the berth, which is walkable: a cruiser holding two tiles off may hover where no SCV can stand
        if (me.currentTile.distanceToIsMore(b, 1)) Orders.MoveToTile(me, b) else Orders.HoldPosition(me)
      }.toList
      else if (raiders(id) && spaceOut(me).isDefined) spaceOut(me).map(Orders.MoveToTile(me, _)).toList
      else if (raiders(id)) {
        // gathering, or ahead of the others: meet them first, so the raid arrives as one
        // only for a while: raiders stuck far behind would hold the leaders back for good
        val cohesive = currentTick - gatheredAt < CohesionFrames
        val meet = centre.filter(c => gathering || cohesive && target.exists(t => waitsForGroup(me.currentTile, c, t)))
        meet.orElse(target).map(Orders.AttackMove(me, _)).toList
      } else if (worldDominationPlan.baseDefenseActive) Nil
      else berth.filter(me.currentTile.distanceToIsMore(_, 8)).map(Orders.AttackMove(me, _)).toList
    }
  }
}
