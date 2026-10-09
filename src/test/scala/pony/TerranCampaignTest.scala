package pony

import org.specs2.Specification
import org.specs2.matcher.MustMatchers
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import pony.brain._
import pony.brain.modules._

import scala.reflect.ClassTag

class TerranCampaignTest extends Specification with MustMatchers {
  def is = s2"""
    Attack starts at inclusive count and live resource thresholds $thresholds
    Expansion keeps a reserve and rejects duplicate, unsafe or unreachable requests $expansion
    Fog and index disappearance retain an observed base $fog
    Seeing only part of a footprint cannot clear a building $partialVisibility
    Destroying a Nexus alone retains the base until surrounding buildings are gone $baseProgression
    Empty visible footprints clear stale structures and permit cleanup $emptyFootprint
    Rebuilt and newly discovered bases are selected without hidden coordinates $rebuilt
    A new match has independent empty campaign state $freshMatch
    Deterministic target ties prefer observed bases then coordinate and id $stableTargets
    Strategy construction does not touch forces before world initialization $initializationOrder
    Paused duplicate callbacks cannot advance AI time and a fresh match resets it $nativeClock
    A distant progressing builder survives pathfinding and a stalled builder expires $builderTravel
    Queued groups safely resolve units which have all died $staleGroup
    Partially dead queued groups retain only surviving members $partialGroup
    Singleton scouting terminates and the final unpaired site is still scouted $singletonScout
    Scouting and offense wait for a staffed expansion and both bank thresholds $economicGate
    Pressure counts the army's minerals and gas together, so marines alone attack $pressureGate
    Starting mineral saturation survives builder departure but resets for a new match $saturation
    Two home depots or old-field cargo cannot establish a second resource field $secondField
    Relocation waits for saturation and queued SCVs then follows native lift and landing $relocation
    SCV reservations count request quantities without duplicating visible training $workerQuota
    Only local mining of the assigned patch proves field operation $localMining
    A ready defensive army preserves its bank through reconnaissance and resumes after launch $stockpile
    Exhausting START cannot revoke a genuinely established second field $exhaustedStart
    Default mineral staffing transfers to landed fields without remote worker demand $coveredStaffing
    Home depots reject the native mineral exclusion zone while ordinary buildings retain placement $depotPlacement
    A refused PlaceBuilding order expires while actual construction remains protected $refusedPlacement
    Native terminal flag batches are separate while genuine live reveal remains invalid $terminalVision
    Both fields retain stable local defenders and replace casualties before expedition admission $defensiveReserve
    Postreserve expedition thresholds keep army production active until the deployable force qualifies $expeditionThresholds
    Local raids invalidate async offense, wait for a busy planner, and clear before offense resumes $defensiveRecall
    A lost guard cannot reserve a fighter still owned by an expedition $reserveCustody
    Mineral field corners require enough nonoverlapping bunker sites for full coverage $bunkerCoverage
    Garrison reservations stay unique and replace a killed Marine without inventing native cargo $bunkerGarrison
    Three separated mineral sectors need three bunkers and actual four-cargo coverage before readiness $threeBunkers
    Greedy coverage retains deterministic safety rejection order without validating lower-ranked sites $bunkerRankedSafety
    Bunker coverage uses pinned native approximate distance and does not claim favorable collision extents $bunkerNativeDistance
    An invalid validated bunker preset refuses generic fallback while legacy placements retain it $strictBunkerSite
    Bunker placement admits temporary traffic but refuses permanent obstacles and severed mining access $bunkerStaticPlacement
    Refused boarding is retried while progressing approaches and loaded Marines receive no reset orders $bunkerBoardingRetry
    The real bunker module refreshes cold garrison caches on completion and destruction ticks $bunkerCacheLifecycle
    Vanished native bunker crews reopen quotas without double counting unfinished training $bunkerCasualtyQuota
    Damaged completed bunkers admit local mineral workers but never gas, builders or unreachable workers $bunkerRepair
    Garrison Marines cannot occupy the mobile defense roster after boarding $bunkerMobileReserve
    Repair healing, target destruction and worker death each terminate in exactly one lifecycle state $bunkerRepairLifecycle
    The real mobile request flow releases repeated jobbed and incomplete dependency funding, preserving completed-producer admission $producerFundingLifecycle
    Mineral coverage includes actual depot return lanes, not only remote patch corners $bunkerWorkerApproaches
    Return routes reject unsolved fields and rasterize solved detours across sparse waypoints $bunkerSolvedRoutes
    Individually admissible bunker sites cannot jointly close a worker corridor $bunkerJointFootprints
    Obsolete loaded home crews cannot fill or suppress active expansion seats $obsoleteBunkerCargo
    Cancelled funded construction is disposed once and its in-flight factory never starts $cancelledConstruction
    A real background placement refusal logs immutable data without reading live worker caches $backgroundPlacementRefusal
    A mineral field at or below five percent is mined out, at or below forty percent no longer held $fieldReplenishment
    A new command center is preferred over moving one that still mines $preferNewDepot
    New bases go to our half of the map first, then close to the main base $expansionSites
    Fully staffed fields ask for one more field, up to the maximum $moreFields
    An endgame hunt sweeps every resource area farthest from home first and wraps around $huntSweep
    A farthest-point carpet spread picks far-apart posts in a deterministic order $carpetPosts
  """
  private def building(id: Int, x: Int, base: Boolean = true) =
    ObservedEnemyBuilding(id, MapTilePosition(x, 20), 4, 3, base)
  def thresholds = {
    val c = TerranCampaignConfig()
    (
      c.launch(12, 1500, 300),
      c.launch(11, 1500, 300),
      c.launch(12, 1499, 300),
      c.launch(12, 1500, 299),
      c.launch(20, 2500, 800)
    ) === (true, false, false, false, true)
  }
  def expansion = {
    val c                                                                      = TerranCampaignConfig()
    def allowed(minerals: Int, pending: Boolean = false, safe: Boolean = true) =
      c.expand(minerals, 0, 400, 0, pending, safe)
    (allowed(400), allowed(399), allowed(400, true), allowed(400, safe = false)) ===
      (true, false, false, false)
  }
  def fog = {
    val m = new EnemyCampaignMemory
    val b = building(1, 30)
    m.update(Seq(b), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0))
    m.update(Nil, Set.empty, _ => false)
    (m.buildings, m.target) === (Vector(b), Some(b.tile))
  }
  def partialVisibility = {
    val m = new EnemyCampaignMemory
    val b = building(1, 30)
    m.update(Seq(b), Set.empty, _ => true)
    m.update(Nil, Set.empty, p => p.x == 30)
    m.buildings === Vector(b)
  }
  def baseProgression = {
    val m       = new EnemyCampaignMemory
    val nexus   = building(1, 30)
    val gateway = building(2, 35, false)
    val other   = building(3, 70)
    m.update(Seq(nexus, gateway, other), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0))
    m.update(Seq(gateway, other), Set(1), _ => false)
    val retained     = m.target
    val nextBuilding = m.attackPosition
    m.update(Seq(other), Set(1, 2), _ => false)
    (retained, nextBuilding, m.select(MapTilePosition(0, 0))) ===
      (Some(nexus.tile), Some(gateway.tile), Some(other.tile))
  }
  def emptyFootprint = {
    val m = new EnemyCampaignMemory
    val b = building(1, 30, false)
    m.update(Seq(b), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0))
    m.update(Nil, Set.empty, _ => true)
    (m.buildings, m.target) === (Vector.empty, None)
  }
  def rebuilt = {
    val m = new EnemyCampaignMemory
    m.update(Seq(building(1, 30)), Set.empty, _ => true)
    m.update(Nil, Set(1), _ => false)
    val rebuilt = building(2, 30)
    m.update(Seq(rebuilt), Set(1), _ => true)
    m.select(MapTilePosition(0, 0)) === Some(rebuilt.tile)
  }
  def freshMatch = {
    val old = new EnemyCampaignMemory
    old.update(Seq(building(1, 30)), Set.empty, _ => true)
    old.select(MapTilePosition(0, 0))
    val fresh = new EnemyCampaignMemory
    (fresh.buildings, fresh.target) === (Vector.empty, None)
  }
  def stableTargets = {
    val m = new EnemyCampaignMemory
    m.update(Seq(building(3, 1, false), building(2, 40), building(1, 30)), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0)) === Some(MapTilePosition(30, 20))
  }
  def initializationOrder = {
    val uninitialized = Proxy.newProxyInstance(
      classOf[Universe].getClassLoader,
      Array[Class[?]](classOf[Universe]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef =
          throw new IllegalStateException("World dependency accessed before initialization: " + method.getName)
      }
    ).asInstanceOf[Universe]
    new strategy.StrategySelector(uninitialized).current.name === "Idle"
  }
  def nativeClock = {
    val c = new NativeFrameClock
    (
      c.advance(0),
      c.advance(0),
      c.advance(0),
      c.advance(1),
      c.advance(1),
      c.advance(0),
      new NativeFrameClock().advance(0)
    ) === (true, false, false, true, false, false, true)
  }
  def builderTravel = {
    val travel = new ConstructionTravelProgress(0, MapTilePosition(0, 0))
    (
      travel.failed(61, MapTilePosition(1, 0), false, false, 60),
      travel.failed(1200, MapTilePosition(50, 0), false, false, 60),
      travel.failed(1921, MapTilePosition(50, 0), false, false, 60),
      travel.failed(1922, MapTilePosition(60, 0), true, false, 60),
      travel.failed(1983, MapTilePosition(60, 0), true, true, 60),
      travel.failed(1984, MapTilePosition(60, 0), true, false, 60)
    ) ===
      (false, false, true, false, false, true)
  }
  def staleGroup = {
    val units = AllUnits(new Units(null, false, null), new Units(null, true, null))
    val group = new Group[WrapsUnit](new Grid2D(10, 10, collection.immutable.BitSet.empty), units)
    (1 to 3).foreach(id => group.add_!(id -> MapTilePosition(id, 0)))
    (group.size, group.survivingMembers) === (3, Vector.empty)
  }
  def partialGroup = {
    val survivor = Proxy.newProxyInstance(
      classOf[WrapsUnit].getClassLoader,
      Array[Class[?]](classOf[WrapsUnit]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef =
          if (method.getName == "nativeUnitId") Int.box(2) else throw new UnsupportedOperationException(method.getName)
      }
    ).asInstanceOf[WrapsUnit]
    val own   = new Units(null, false, null) { override def byId(id: Int) = if (id == 2) Some(survivor) else None }
    val group = new Group[WrapsUnit](
      new Grid2D(10, 10, collection.immutable.BitSet.empty),
      AllUnits(own, new Units(null, true, null))
    )
    (1 to 3).foreach(id => group.add_!(id -> MapTilePosition(id, 0)))
    group.survivingMembers.map(_.nativeUnitId) === Vector(2)
  }
  def singletonScout = {
    val a                                                                    = MapTilePosition(1, 1)
    val b                                                                    = MapTilePosition(2, 2)
    def next(points: Vector[MapTilePosition], covered: Set[MapTilePosition]) =
      ScoutPointPairs.next(points, covered)((_, _) => Some(1.0))
    (next(Vector(a), Set.empty), next(Vector(a), Set(a)), next(Vector(a, b), Set(a))) ===
      (List(a), Nil, List(b))
  }
  def pressureGate = {
    val c = TerranCampaignConfig()
    (c.pressure(12, 1800, 0), c.pressure(12, 1500, 299), c.pressure(11, 5000, 5000)) === (true, false, false)
  }

  def economicGate = {
    val c = TerranCampaignConfig()
    (
      c.ready(false, 12, 1500, 300, 1000, 300),
      c.ready(true, 12, 1500, 300, 999, 300),
      c.ready(true, 12, 1500, 300, 1000, 299),
      c.ready(true, 11, 1500, 300, 1000, 300),
      c.ready(true, 12, 1500, 300, 1000, 300)
    ) === (false, false, false, false, true)
  }
  def moreFields = {
    import pony.brain.modules.ExpansionChoice.wantedFields
    (
      wantedFields(2, 1, allSaturated = true, max = 5),
      wantedFields(2, 2, allSaturated = false, max = 5),
      wantedFields(2, 2, allSaturated = true, max = 5),
      wantedFields(2, 4, allSaturated = true, max = 5),
      wantedFields(2, 5, allSaturated = true, max = 5)
    ) === (2, 2, 3, 5, 5)
  }

  def expansionSites = {
    import pony.brain.modules.ExpansionSite._
    // our start at the bottom, the enemy at the top; field 3 is closest to the main but on the enemy's half
    val far    = Candidate(1, 10, 100, defenseLine = false)
    val near   = Candidate(2, 80, 96, defenseLine = false)
    val theirs = Candidate(3, 50, 40, defenseLine = true)
    val lined  = Candidate(4, 80, 96, defenseLine = true)
    rank(Seq(far, near, theirs, lined), main = (64, 70), ourStart = (64, 118), enemyStarts = Seq((31, 7)))
      .map(_.id) === Seq(4, 2, 1, 3)
  }

  def preferNewDepot = {
    import pony.brain.modules.ExpansionChoice._
    (
      decide(0, Nil, Seq(5), newUnderWay = false, affordable = true),
      decide(1, Seq(9), Seq(5), newUnderWay = false, affordable = true),
      decide(1, Nil, Seq(5), newUnderWay = true, affordable = false),
      decide(1, Nil, Seq(5), newUnderWay = false, affordable = true),
      decide(1, Nil, Seq(5), newUnderWay = false, affordable = false),
      decide(1, Nil, Nil, newUnderWay = false, affordable = false)
    ) === (Hold, Move(9), Hold, BuildNew, Move(5), WaitForMinerals)
  }

  def fieldReplenishment = {
    val c      = TerranCampaignConfig()
    val custom = TerranCampaignConfig(fieldUsefulFraction = 0.25)
    (
      c.fieldUseful(1.0),
      c.fieldUseful(0.050001),
      c.fieldUseful(0.05),
      c.fieldUseful(0.049),
      c.fieldUseful(0.0),
      custom.fieldUseful(0.26),
      custom.fieldUseful(0.24),
      custom.fieldUseful(0.25)
    ) ===
      (true, true, false, false, false, true, false, false) and
      ((c.fieldHealthy(0.41), c.fieldHealthy(0.4), c.fieldUseful(0.3)) === (true, false, true))
  }
  def saturation = {
    val progress = new TerranEconomicProgress
    progress.observe(Some(1), Seq(MiningFieldStatus(1, 20, 19, 10, true)))
    val early = progress.startingFieldSaturated
    progress.observe(Some(1), Seq(MiningFieldStatus(1, 20, 20, 10, true)))
    progress.observe(Some(1), Seq(MiningFieldStatus(1, 20, 19, 10, true)))
    (early, progress.startingFieldSaturated, new TerranEconomicProgress().startingFieldSaturated) ===
      (false, true, false)
  }
  def secondField = {
    val p    = new TerranEconomicProgress
    val home = MiningFieldStatus(1, 20, 20, 5, true)
    p.observe(Some(1), Seq(home))
    val flying                                  = MiningFieldStatus(2, 20, 20, 5, false)
    val unworked                                = MiningFieldStatus(2, 20, 10, 0, true)
    def observe(fields: Seq[MiningFieldStatus]) = { p.observe(Some(1), fields); p.secondBaseEstablished }
    (
      observe(Seq(home, home)),
      observe(Seq(home, flying)),
      observe(Seq(home, unworked)),
      observe(Seq(home, unworked.copy(working = 1)))
    ) === (false, false, false, true)
  }
  def relocation = {
    import DepotRelocation._
    (
      next(false, false, false, false, false),
      next(true, true, false, false, false),
      next(true, false, false, false, false),
      next(true, false, true, false, false),
      next(true, false, true, true, false),
      next(true, false, false, true, true),
      next(false, false, true, false, false)
    ) ===
      (AwaitSaturation, FinishTraining, Lift, Fly, Land, Established, Fly)
  }
  def huntSweep = {
    import HuntSweep._
    val home = Some(MapTilePosition(10, 10))
    val near = MapTilePosition(11, 11)
    val mid  = MapTilePosition(30, 30)
    val far  = MapTilePosition(50, 50)
    (
      order(Seq(near, mid, far), home),
      order(Seq(near, mid, far), None),
      next(3, 2),
      next(3, 0),
      next(0, 2)
    ) ===
      (Vector(far, mid, near), Vector(near, mid, far), 0, 1, 2)
  }
  def carpetPosts = {
    import CarpetPosts._
    val corners = Vector(
      MapTilePosition(0, 0),
      MapTilePosition(1, 0),
      MapTilePosition(10, 0),
      MapTilePosition(11, 0)
    )
    (
      order(corners, 2),
      order(corners, 4),
      order(Vector.empty, 3),
      order(Vector(MapTilePosition(5, 5)), 2)
    ) ===
      (
        Vector(MapTilePosition(0, 0), MapTilePosition(11, 0)),
        Vector(MapTilePosition(0, 0), MapTilePosition(11, 0), MapTilePosition(1, 0), MapTilePosition(10, 0)),
        Vector.empty[MapTilePosition],
        Vector(MapTilePosition(5, 5))
      )
  }
  def workerQuota = {
    def missing(incomplete: Int, reserved: Int, native: Int, requests: Seq[Int]) =
      WorkerProductionQuota.missing(24, 20, incomplete, reserved, native, requests)
    (
      missing(0, 2, 0, Nil),
      missing(2, 2, 2, Nil),
      missing(0, 0, 2, Nil),
      missing(0, 0, 0, Seq(3)),
      missing(2, 2, 2, Seq(3))
    ) === (2, 2, 2, 1, 0)
  }
  def localMining = {
    (
      LocalMineralMining.observed(false, 11, Some(11), true),
      LocalMineralMining.observed(true, 11, Some(9), true),
      LocalMineralMining.observed(true, 11, None, true),
      LocalMineralMining.observed(true, 11, Some(11), false),
      LocalMineralMining.observed(true, 11, Some(11), true)
    ) === (false, false, false, false, true)
  }
  def stockpile = {
    val c = TerranCampaignConfig()
    (
      c.holdNewArmy(false, 12, 1500, 300, false),
      c.holdNewArmy(true, 11, 1500, 300, false),
      c.holdNewArmy(true, 12, 1500, 300, false),
      c.holdNewArmy(true, 12, 1500, 300, true)
    ) ===
      (false, false, true, false)
  }
  def exhaustedStart = {
    val p    = new TerranEconomicProgress
    val home = MiningFieldStatus(1, 20, 20, 5, true)
    val next = MiningFieldStatus(2, 14, 5, 1, true)
    p.observe(Some(1), Seq(home))
    p.observe(Some(1), Seq(home, next))
    p.observe(Some(1), Seq(home.copy(capacity = 0, assigned = 0, working = 0), next))
    val c = TerranCampaignConfig()
    (
      p.startingFieldSaturated,
      p.secondBaseEstablished,
      c.ready(p.secondBaseEstablished, 30, 3000, 1000, 1000, 300),
      new TerranEconomicProgress().secondBaseEstablished
    ) === (true, true, true, false)
  }
  def coveredStaffing = {
    // A relocated depot serves field2; the old field and distant field3 must not enqueue workers.
    val requests                                      = Seq(1 -> 20, 2 -> 14, 3 -> 60)
    def demand(default: Boolean, landed: Option[Int]) = requests.filter { case (field, _) =>
      MineralFieldStaffing.permitted(default, landed, field)
    }.map(_._2).sum
    (
      demand(true, Some(2)),
      demand(true, None),
      demand(false, Some(2)),
      WorkerProductionQuota.missing(demand(true, Some(2)) + 6, 24, 0, 0, 0, Nil)
    ) ===
      (14, 0, 94, 0)
  }
  def depotPlacement = {
    val blocked = new Grid2D(128, 128, collection.immutable.BitSet.empty).mutableCopy
    blocked.block_!(Area(MapTilePosition(71, 115), Size(2, 1)).growBy(3))
    val refusedNativeSite = Area(MapTilePosition(69, 110), Size(4, 3))
    val clearHomeSite     = Area(MapTilePosition(55, 108), Size(4, 3))
    (
      ResourceDepotPlacement.permitted(refusedNativeSite, true, blocked),
      ResourceDepotPlacement.permitted(clearHomeSite, true, blocked),
      ResourceDepotPlacement.permitted(refusedNativeSite, false, blocked)
    ) === (false, true, true)
  }
  def refusedPlacement = {
    val refused  = new ConstructionTravelProgress(8323, MapTilePosition(70, 115))
    val building = new ConstructionTravelProgress(8323, MapTilePosition(69, 110))
    // PlaceBuilding is an attempted command, not native ConstructingBuilding.
    (
      refused.failed(8324, MapTilePosition(70, 115), false, false, 60),
      refused.failed(9044, MapTilePosition(70, 115), false, false, 60),
      building.failed(8324, MapTilePosition(69, 110), true, true, 60),
      building.failed(12000, MapTilePosition(69, 110), true, true, 60)
    ) === (false, true, false, false)
  }
  def terminalVision = {
    val fair     = new NativeVisionCoverage
    val live     = fair.observe(100, true, false, false)
    val terminal = fair.observe(101, true, true, true)
    val invalid  = new NativeVisionCoverage
    invalid.observe(100, true, false, true) // No native victory/defeat proof: this is real invalid coverage.
    invalid.observe(101, true, true, false)
    (
      live,
      terminal,
      fair.liveSamples,
      fair.lastLiveFrame,
      fair.terminalSamples,
      fair.terminalCompleteMap,
      fair.ordinaryVision,
      invalid.ordinaryVision,
      new NativeVisionCoverage().ordinaryVision
    ) ===
      (true, false, 1, 100, 1, true, true, false, false)
  }
  def defensiveReserve = {
    val roster = new TerranDefenseRoster(2)
    val fields = Seq(DefenseField(1, MapTilePosition(10, 10)), DefenseField(2, MapTilePosition(80, 80)))
    val troops = (1 to 3).map(id => DefenseFighter(id, MapTilePosition(10 + id, 10))) ++
      (4 to 6).map(id => DefenseFighter(id, MapTilePosition(80 + id, 80)))
    roster.update(fields, troops)
    val first      = roster.reserved
    val deployable = troops.map(_.id).filterNot(first)
    roster.update(fields, troops.filterNot(_.id == 1))
    (first, deployable, roster.reserved, roster.rallyFor(4), roster.rallyFor(6)) ===
      (Set(1, 2, 4, 5), Vector(3, 6), Set(2, 3, 4, 5), Some(MapTilePosition(80, 80)), None)
  }
  def expeditionThresholds = {
    val c          = TerranCampaignConfig()
    val total      = 18
    val reserved   = 12
    val expedition = total - reserved
    (
      c.ready(true, expedition, 600, 200, 2000, 1000),
      c.holdNewArmy(true, expedition, 600, 200, false),
      c.ready(true, 12, 1500, 300, 1000, 300),
      c.holdNewArmy(true, 12, 1500, 300, false)
    ) === (false, false, true, true)
  }
  def defensiveRecall = {
    val control          = new CampaignDefenseControl
    val offensiveVersion = control.generation
    control.setPressure(true)
    val raid = MapTilePosition(10, 10)
    control.queue(raid)
    val duringPlanning = control.takeReady(true)
    val recalled       = control.takeReady(false)
    val staleRejected  = !control.acceptsCampaign(offensiveVersion)
    val whileRaided    = control.acceptsCampaign(control.generation)
    control.setPressure(false)
    (
      duringPlanning,
      recalled,
      staleRejected,
      whileRaided,
      control.acceptsCampaign(offensiveVersion),
      control.acceptsCampaign(control.generation),
      control.takeReady(false)
    ) === (None, Some(raid), true, false, false, true, None)
  }
  def reserveCustody = {
    val roster = new TerranDefenseRoster(1)
    val field  = Seq(DefenseField(1, MapTilePosition(10, 10)))
    roster.update(field, Seq(DefenseFighter(1, MapTilePosition(10, 10))))
    roster.update(field, Seq(DefenseFighter(2, MapTilePosition(11, 10), campaignAssigned = true)))
    val whileAway = roster.reserved
    roster.update(
      field,
      Seq(
        DefenseFighter(2, MapTilePosition(11, 10), campaignAssigned = true),
        DefenseFighter(3, MapTilePosition(12, 10))
      )
    )
    (whileAway, roster.reserved) === (Set.empty[Int], Set(3))
  }
  def bunkerCoverage = {
    val left           = Area(MapTilePosition(10, 10), Size(3, 2))
    val right          = Area(MapTilePosition(18, 10), Size(3, 2))
    val corners        = BunkerCoverage.corners(Seq(MapTilePosition(8, 10), MapTilePosition(22, 10)))
    val selected       = BunkerCoverage.select(corners, Vector(left, right), Vector.empty, 160)
    val oneCannotCover = BunkerCoverage.select(corners, Vector(left), Vector.empty, 160)
    val single         = BunkerCoverage.select(
      BunkerCoverage.corners(Seq(MapTilePosition(10, 12))),
      Vector(left, right),
      Vector.empty,
      160
    )
    (
      selected.size,
      corners.forall(p => selected.exists(BunkerCoverage.covers(_, p, 160))),
      oneCannotCover,
      single.size
    ) === (2, true, Vector.empty, 1)
  }
  def bunkerGarrison = {
    val g       = new BunkerGarrison
    val homes   = Seq((100, MapTilePosition(10, 10), Set(1, 2)), (200, MapTilePosition(80, 80), Set.empty[Int]))
    val marines = (1 to 4).map(i => i -> MapTilePosition(10, 10)) ++
      (5 to 8).map(i => i -> MapTilePosition(80, 80))
    g.update(homes, marines)
    val before = g.reserved
    g.update(
      homes.map { case (id, p, cargo) => (id, p, cargo - 1) },
      marines.filterNot(_._1 == 1) :+ (9 -> MapTilePosition(11, 10))
    )
    (before, g.reserved, g.target(9), g.target(5)) ===
      (Set(1, 2, 3, 4, 5, 6, 7, 8), Set(2, 3, 4, 5, 6, 7, 8, 9), Some(100), Some(200))
  }
  def threeBunkers = {
    val points = BunkerCoverage.corners(Seq(MapTilePosition(8, 10), MapTilePosition(18, 10), MapTilePosition(30, 10)))
    val candidates = Vector(10, 18, 26).map(x => Area(MapTilePosition(x, 10), Size(3, 2)))
    val selected   = BunkerCoverage.select(points, candidates, Vector.empty, 160)
    val full       = selected.map(_.upperLeft -> 4).toMap
    (
      selected.size,
      BunkerCoverage.ready(points, selected, Map.empty, 160),
      BunkerCoverage.ready(points, selected, full.updated(selected.head.upperLeft, 3), 160),
      BunkerCoverage.ready(points, selected, full, 160)
    ) === (3, false, false, true)
  }
  def bunkerRankedSafety = {
    def site(x: Int, y: Int) = Area(MapTilePosition(x, y), Size(3, 2))
    val a                    = site(10, 10); val b = site(30, 10); val c = site(50, 10)
    val rejectedFirst        = site(9, 10)
    val alternateB           = site(30, 11)
    val points               = Vector(a, b, c).map(s => MapPosition(s.upperLeft.mapX + 48, s.upperLeft.mapY + 32))
    // All three sectors need coverage, so the single/pair shortcuts cannot satisfy this field.
    val lowerRanked = (11 to 14).flatMap(y => Vector(10, 30, 50).map(x => site(x, y))).toVector
    val candidates  = Vector(a, b, c, rejectedFirst) ++ lowerRanked
    def run(input: Vector[Area], jointRefusal: Boolean) = {
      var checks   = Vector.empty[Vector[Area]]
      val selected = BunkerCoverage.select(
        points,
        input,
        Vector.empty,
        96,
        together => {
          val set = together.toVector
          checks :+= set
          !set.contains(rejectedFirst) && !(jointRefusal && set.contains(a) && set.contains(b))
        }
      )
      (selected, checks.size, points.forall(p => selected.exists(BunkerCoverage.covers(_, p, 96))))
    }
    (
      run(candidates, false),
      run(candidates.reverse, false),
      run(candidates, true),
      run(candidates.reverse, true)
    ) ===
      (
        (Vector(a, b, c), 4, true),
        (Vector(a, b, c), 4, true),
        (Vector(a, c, alternateB), 6, true),
        (Vector(a, c, alternateB), 6, true)
      )
  }
  def bunkerNativeDistance = {
    (
      BunkerCoverage.approximateDistance(MapPosition(0, 0), MapPosition(160, 0)),
      BunkerCoverage.approximateDistance(MapPosition(0, 0), MapPosition(160, 160)),
      BunkerCoverage.approximateDistance(MapPosition(160, 160), MapPosition(0, 0))
    ) === (160, 209, 209)
  }
  def strictBunkerSite = {
    val site       = MapTilePosition(20, 20)
    val refused    = AlternativeBuildingSpot.fromValidatedPreset(site)(false)
    val accepted   = AlternativeBuildingSpot.fromValidatedPreset(site)(true)
    val pending    = Seq(refused, accepted).flatMap(_.requestedPosition).toSet
    val unresolved = accepted.predefined
    refused.init_!(); accepted.init_!()
    (
      refused.resolve(Some(MapTilePosition(90, 90))),
      accepted.resolve(Some(MapTilePosition(90, 90))),
      AlternativeBuildingSpot.useDefault.resolve(Some(MapTilePosition(90, 90))),
      pending,
      unresolved
    ) ===
      (None, Some(site), Some(MapTilePosition(90, 90)), Set(site), None)
  }
  def bunkerStaticPlacement = {
    val clear   = new Grid2D(12, 12, collection.immutable.BitSet.empty)
    val site    = Area(MapTilePosition(5, 3), Size(3, 2))
    val traffic = clear.mutableCopy
    traffic.block_!(site)
    val adjacent = clear.mutableCopy
    adjacent.block_!(Area(MapTilePosition(4, 2), Size(1, 2)))
    val corridor = new Grid2D(12, 6, collection.immutable.BitSet.empty).mutableCopy
    corridor.block_!(Area(MapTilePosition(0, 0), Size(12, 2)))
    corridor.block_!(Area(MapTilePosition(0, 4), Size(12, 2)))
    (
      BunkerSitePlacement.permitted(site, clear),
      traffic.free(site),
      BunkerSitePlacement.permitted(site, traffic),
      BunkerSitePlacement.permitted(site, adjacent),
      BunkerSitePlacement.permitted(Area(MapTilePosition(5, 2), Size(3, 2)), corridor)
    ) ===
      (true, false, false, true, false)
  }
  def bunkerBoardingRetry = {
    val retry   = new BunkerBoardingRetry
    val initial = MapTilePosition(10, 10)
    // First native command may be refused: unchanged target/state must get another command.
    (
      retry.issue(0, initial, false, false, false),
      retry.issue(1, initial, false, false, false),
      retry.issue(12, initial, false, false, false),
      retry.issue(24, MapTilePosition(11, 10), false, true, true),
      retry.issue(100, MapTilePosition(12, 10), false, true, true),
      retry.issue(220, MapTilePosition(12, 10), false, true, true),
      retry.issue(240, MapTilePosition(12, 10), true, false, false)
    ) ===
      (true, false, true, false, false, true, false)
  }
  def bunkerCacheLifecycle = {
    var frame    = 0
    val universe = Proxy.newProxyInstance(
      classOf[Universe].getClassLoader,
      Array[Class[?]](classOf[Universe]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = method.getName match {
          case "register_$bang" => null
          case "currentTick"    => Int.box(frame)
          case other            => throw new IllegalStateException("Unexpected native dependency: " + other)
        }
      }
    ).asInstanceOf[Universe]
    val module = new TerranBunkerDefense(universe) {
      override def race     = pony.Terran
      override val strategy = new modules.strategy.StrategySelector(this.universe)
    }
    val garrison  = new BunkerGarrison
    var completed = false
    val snapshot  = module.oncePerTick {
      val bunkers = if (completed) Seq((100, MapTilePosition(10, 10), Set.empty[Int])) else Nil
      garrison.update(bunkers, (1 to 4).map(_ -> MapTilePosition(10, 10)))
      garrison.reserved
    }
    val cold = snapshot.get
    completed = true; frame = 1; module.onTick_!()
    val afterCompletion = snapshot.get
    completed = false; frame = 2; module.onTick_!()
    (cold, afterCompletion, snapshot.get) === (Set.empty[Int], Set(1, 2, 3, 4), Set.empty[Int])
  }
  def bunkerCasualtyQuota = {
    val before = (1 to 8).toSet
    val after  = before -- (5 to 8)
    (
      BunkerMarineQuota.missing(8, before, Set.empty, 0, Nil),
      BunkerMarineQuota.missing(8, after, Set.empty, 0, Nil),
      BunkerMarineQuota.missing(8, after, Set(9), 1, Seq(1)),
      BunkerMarineQuota.missing(8, Set.empty, Set.empty, 0, Nil),
      BunkerMarineQuota.missing(8, before, Set.empty, 0, Nil, campaignHeld = before)
    ) === (0, 4, 2, 8, 8)
  }
  def bunkerRepair = {
    def admit(
        damaged: Boolean = true,
        completed: Boolean = true,
        alive: Boolean = true,
        local: Boolean = true,
        mineralOrIdle: Boolean = true
    ) =
      BunkerRepairAdmission.eligible(damaged, completed, alive, local, mineralOrIdle)
    (
      admit(),
      admit(damaged = false),
      admit(completed = false),
      admit(alive = false),
      admit(local = false),
      admit(mineralOrIdle = false)
    ) === (true, false, false, false, false, false)
  }
  def bunkerMobileReserve = {
    val roster = new TerranDefenseRoster(2)
    val field  = Seq(DefenseField(1, MapTilePosition(10, 10)))
    roster.update(field, Seq(DefenseFighter(1, MapTilePosition(10, 10)), DefenseFighter(2, MapTilePosition(10, 10))))
    roster.update(
      field,
      Seq(
        DefenseFighter(1, MapTilePosition(10, 10), garrisonReserved = true),
        DefenseFighter(2, MapTilePosition(10, 10), garrisonReserved = true),
        DefenseFighter(3, MapTilePosition(11, 10)),
        DefenseFighter(4, MapTilePosition(12, 10))
      )
    )
    (roster.reserved, roster.rallyFor(1)) === (Set(3, 4), None)
  }
  def bunkerRepairLifecycle = {
    import BunkerRepairState._
    (
      BunkerRepairState(true, true, true, false),
      BunkerRepairState(true, true, false, false),
      BunkerRepairState(true, false, true, false),
      BunkerRepairState(false, true, false, false),
      BunkerRepairState(false, false, true, false),
      BunkerRepairState(true, true, true, true)
    ) ===
      (Repairing, Finished, Finished, Failed, Failed, Failed)
  }
  def producerFundingLifecycle = {
    var ledger: ResourceManager = null
    var manager: UnitManager    = null
    val universe                = Proxy.newProxyInstance(
      classOf[Universe].getClassLoader,
      Array[Class[?]](classOf[Universe]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = method.getName match {
          case "register_$bang" => null
          case "resources"      => ledger
          case "unitManager"    => manager
          case "currentTick"    => Int.box(0)
          case other            => throw new IllegalStateException("Unexpected native dependency: " + other)
        }
      }
    ).asInstanceOf[Universe]
    val locks   = scala.collection.mutable.Map.empty[ResourceApprovalSuccess, ResourceRequestSum]
    val holders = scala.collection.mutable.Map.empty[ResourceApproval, HasFunding]
    var serial  = 0
    ledger = new ResourceManager(universe) {
      override def request[T <: WrapsUnit](
          cost: ResourceRequests,
          employer: Employer[T],
          lock: Boolean
      ): ResourceApproval = {
        serial += 1
        val proof = ResourceApprovalSuccess(cost.minerals, cost.gas, cost.supply, ResourceApprovalId(serial))
        locks(proof) = cost.sum
        proof
      }
      override def informUsage[T <: WrapsUnit](proof: ResourceApproval, owner: HasFunding): Unit = {
        require(locks.contains(proof.assumeSuccessful))
        holders(proof) = owner
      }
      override def unlock_!(proof: ResourceApprovalSuccess): Unit = {
        require(locks.remove(proof).isDefined, "Exact funding must be released only once")
        holders.remove(proof)
      }
    }
    var producer = "jobbed"
    val pending  = scala.collection.mutable.ArrayBuffer.empty[UnitJobRequest[?]]
    manager = new UnitManager(universe) {
      override def request[T <: WrapsUnit: ClassTag](
          req: UnitJobRequest[T],
          buildIfNoneAvailable: Boolean
      ): PreHiringResult[T] = {
        val barracks = Set[Class[? <: Building]](classOf[Barracks])
        producer match {
          case "jobbed"     => new MissingRequirementResult[T](Set.empty, Set.empty, Set.empty, barracks)
          case "incomplete" => new MissingRequirementResult[T](Set.empty, barracks, Set.empty, Set.empty)
          case _            => pending += req; new FailedPreHiringResult[T]
        }
      }
    }
    val owner  = new Employer[Mobile](universe)
    val module = new HelperAIModule[UnitFactory](universe) with UnitRequestHelper {
      override protected def mobileCost[T <: Mobile](kind: Class[? <: T], priority: Priority) =
        ResourceRequests(Seq(MineralsRequest(50), SupplyRequest(2)), priority, kind)
      override protected def mobileRequest[T <: Mobile](
          kind: Class[? <: T],
          proof: ResourceApprovalSuccess,
          priority: Priority
      ) = {
        val request =
          BuildUnitRequest[Mobile](this.universe, kind, 1, proof, priority, AlternativeBuildingSpot.useDefault)
        request.persistant_!()
        UnitJobRequest[Mobile](request, owner, priority)
      }
    }
    val jobbed      = (1 to 100).map(_ => module.requestUnit(classOf[Marine], takeCareOfDependencies = false))
    val afterJobbed = (locks.size, holders.size, pending.size)
    producer = "incomplete"
    val incomplete      = (1 to 100).map(_ => module.requestUnit(classOf[Marine], takeCareOfDependencies = false))
    val afterIncomplete = (locks.size, holders.size, pending.size)
    producer = "completed"
    val admitted = module.requestUnit(classOf[Marine], takeCareOfDependencies = false)
    (
      jobbed.forall(!_),
      incomplete.forall(!_),
      afterJobbed,
      afterIncomplete,
      admitted,
      pending.size,
      holders.size,
      locks.values.map(_.minerals).sum,
      locks.values.map(_.supply).sum
    ) ===
      (true, true, (0, 0, 0), (0, 0, 0), true, 1, 1, 50, 2)
  }
  def bunkerWorkerApproaches = {
    val patch = Area(MapTilePosition(25, 10), Size(2, 1))
    val depot = Area(MapTilePosition(8, 9), Size(4, 3))
    val tiles =
      BunkerCoverage.workerTiles(Seq(patch), Seq(depot), Seq(Vector(MapTilePosition(11, 10), MapTilePosition(24, 10))))
    val points           = BunkerCoverage.corners(tiles)
    val onlyRemoteBunker = Area(MapTilePosition(25, 12), Size(3, 2))
    (
      tiles.contains(MapTilePosition(17, 10)),
      depot.outline.forall(tiles.contains),
      points.forall(BunkerCoverage.covers(onlyRemoteBunker, _, 192)),
      BunkerCoverage.workerTiles(Seq(patch), Nil, Nil).contains(MapTilePosition(17, 10))
    ) ===
      (true, true, false, false)
  }
  def bunkerSolvedRoutes = {
    val grid = new Grid2D(32, 24, collection.immutable.BitSet.empty).mutableCopy
    grid.block_!(Area(MapTilePosition(18, 0), Size(1, 15)))
    val from   = MapTilePosition(11, 10); val to = MapTilePosition(24, 10)
    val detour = BunkerWorkerRoutes.between(from, to, grid).get
    val tiles  = BunkerCoverage.workerTiles(Nil, Nil, Seq(detour))
    grid.block_!(Area(MapTilePosition(18, 15), Size(1, 9)))
    (
      detour.head,
      detour.last,
      tiles.exists(_.y >= 15),
      BunkerWorkerRoutes.between(from, to, grid)
    ) === (from, to, true, None)
  }
  def bunkerJointFootprints = {
    val corridor = new Grid2D(16, 10, collection.immutable.BitSet.empty).mutableCopy
    corridor.block_!(Area(MapTilePosition(0, 0), Size(16, 3)))
    corridor.block_!(Area(MapTilePosition(0, 7), Size(16, 3)))
    val a = Area(MapTilePosition(6, 3), Size(3, 2))
    val b = Area(MapTilePosition(6, 5), Size(3, 2))
    (
      BunkerSitePlacement.permitted(a, corridor),
      BunkerSitePlacement.permitted(b, corridor),
      BunkerSitePlacement.permittedTogether(Seq(a, b), corridor)
    ) === (true, true, false)
  }
  def obsoleteBunkerCargo = {
    val garrison = new BunkerGarrison
    val obsolete = (1 to 4).toSet
    val active   = (5 to 8).toSet
    val home     = MapTilePosition(10, 10); val expansion = MapTilePosition(30, 30)
    garrison.update(Seq((100, home, obsolete)), obsolete.toVector.map(_ -> home))
    garrison.update(
      Seq((200, expansion, active), (201, MapTilePosition(34, 30), Set.empty[Int])),
      active.toVector.map(_ -> expansion)
    )
    val missing = BunkerMarineQuota.missing(
      8,
      obsolete ++ active,
      Set.empty,
      0,
      Nil,
      obsoleteCargo = obsolete
    )
    val fresh = (9 to 12).toSet
    garrison.update(
      Seq((200, expansion, active), (201, MapTilePosition(34, 30), Set.empty[Int])),
      (active ++ fresh).toVector.map(_ -> expansion)
    )
    (
      missing,
      obsolete.exists(garrison.reserved),
      fresh.forall(id => garrison.target(id).contains(201)),
      BunkerMarineQuota.missing(8, obsolete ++ active ++ fresh, Set.empty, 0, Nil, obsoleteCargo = obsolete)
    ) ===
      (4, false, true, 0)
  }
  def cancelledConstruction = {
    var ledger: ResourceManager = null
    val universe                = Proxy.newProxyInstance(
      classOf[Universe].getClassLoader,
      Array[Class[?]](classOf[Universe]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = method.getName match {
          case "register_$bang" => null
          case "resources"      => ledger
          case other            => throw new IllegalStateException("Unexpected native dependency: " + other)
        }
      }
    ).asInstanceOf[Universe]
    var locked = true; var released = 0; var created = 0
    ledger = new ResourceManager(universe) {
      override def informUsage[T <: WrapsUnit](proof: ResourceApproval, owner: HasFunding): Unit = {}
      override def hasStillLocked(proof: ResourceApproval)                                       = locked
      override def unlock_!(proof: ResourceApprovalSuccess): Unit                                = {
        require(locked); locked = false; released += 1
      }
    }
    val proof   = ResourceApprovalSuccess(100, 0, 0, ResourceApprovalId(1))
    val request = BuildUnitRequest[Building](
      universe,
      classOf[Bunker],
      1,
      proof,
      Priority.Default,
      AlternativeBuildingSpot.useDefault
    )
    request.persistant_!()
    val computation = BackgroundComputationResult.result[WorkerUnit, UnitWithJob[WorkerUnit]](
      Seq(() => { created += 1; throw new IllegalStateException("Cancelled construction must never start") }),
      () => false,
      () => !request.clearable && ledger.hasStillLocked(proof)
    )(_ => ())
    request.forceUnlockOnDispose_!(); request.clearableInNextTick_!()
    computation.afterComputation()
    val noJobs = computation.jobs.isEmpty
    request.dispose()
    (noJobs, computation.jobs.isEmpty, created, released, locked) === (true, true, 0, 1, false)
  }
  def backgroundPlacementRefusal = {
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._
    var ledger: ResourceManager = null
    val universe                = Proxy.newProxyInstance(
      classOf[Universe].getClassLoader,
      Array[Class[?]](classOf[Universe]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = method.getName match {
          case "register_$bang" => null
          case "resources"      => ledger
          case other            => throw new IllegalStateException("Unexpected native dependency: " + other)
        }
      }
    ).asInstanceOf[Universe]
    var released = 0
    ledger = new ResourceManager(universe) {
      override def informUsage[T <: WrapsUnit](proof: ResourceApproval, owner: HasFunding): Unit = {}
      override def unlock_!(proof: ResourceApprovalSuccess): Unit                                = { released += 1 }
    }
    val nativeThread = Thread.currentThread()
    val worker       = Proxy.newProxyInstance(
      classOf[WorkerUnit].getClassLoader,
      Array[Class[?]](classOf[WorkerUnit]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = method.getName match {
          case "nativeUnitId" => require(Thread.currentThread() == nativeThread); Int.box(274)
          case "toString"     => throw new AssertionError("A worker diagnostic would read thread-bound currentTile")
          case other          => throw new IllegalStateException("Unexpected worker access: " + other)
        }
      }
    ).asInstanceOf[WorkerUnit]
    val module = new ProvideNewBuildings(universe) {
      override protected def constructionSite(in: Data) = None
    }
    val request = BuildUnitRequest[Building](
      universe,
      classOf[Bunker],
      1,
      ResourceApprovalSuccess(100, 0, 0, ResourceApprovalId(2)),
      Priority.Default,
      AlternativeBuildingSpot.useDefault
    )
    request.persistant_!()
    val input                = new module.Data(worker, classOf[Bunker], MapTilePosition(64, 118), null, request)
    val (diagnostic, result) = Await.result(
      Future {
        input.toString -> module.evaluateNextOrders(input)
      }(using scala.concurrent.ExecutionContext.Implicits.global),
      5.seconds
    )
    result.afterComputation(); request.dispose()
    (diagnostic, result.jobs.isEmpty, released) ===
      ("ConstructionData(worker=274, building=Bunker, home=(64,118))", true, 1)
  }
}
