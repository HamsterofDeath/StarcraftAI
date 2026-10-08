package pony

import org.specs2.Specification
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import pony.brain.{Universe, ConstructionTravelProgress}
import pony.brain.modules._

class TerranCampaignTest extends Specification {
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
  """
  private def building(id: Int, x: Int, base: Boolean = true) =
    ObservedEnemyBuilding(id, MapTilePosition(x, 20), 4, 3, base)
  def thresholds = {
    val c = TerranCampaignConfig()
    (c.launch(12, 1500, 300), c.launch(11, 1500, 300), c.launch(12, 1499, 300),
      c.launch(12, 1500, 299), c.launch(20, 2500, 800)) mustEqual (true, false, false, false, true)
  }
  def expansion = {
    val c = TerranCampaignConfig()
    def allowed(minerals: Int, pending: Boolean = false, safe: Boolean = true) =
      c.expand(minerals, 0, 400, 0, pending, safe)
    (allowed(400), allowed(399), allowed(400, true), allowed(400, safe = false)) mustEqual
      (true, false, false, false)
  }
  def fog = {
    val m = new EnemyCampaignMemory
    val b = building(1, 30)
    m.update(Seq(b), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0))
    m.update(Nil, Set.empty, _ => false)
    (m.buildings, m.target) mustEqual (Vector(b), Some(b.tile))
  }
  def partialVisibility = {
    val m = new EnemyCampaignMemory
    val b = building(1, 30)
    m.update(Seq(b), Set.empty, _ => true)
    m.update(Nil, Set.empty, p => p.x == 30)
    m.buildings mustEqual Vector(b)
  }
  def baseProgression = {
    val m = new EnemyCampaignMemory
    val nexus = building(1, 30)
    val gateway = building(2, 35, false)
    val other = building(3, 70)
    m.update(Seq(nexus, gateway, other), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0))
    m.update(Seq(gateway, other), Set(1), _ => false)
    val retained = m.target
    val nextBuilding = m.attackPosition
    m.update(Seq(other), Set(1, 2), _ => false)
    (retained, nextBuilding, m.select(MapTilePosition(0, 0))) mustEqual
      (Some(nexus.tile), Some(gateway.tile), Some(other.tile))
  }
  def emptyFootprint = {
    val m = new EnemyCampaignMemory
    val b = building(1, 30, false)
    m.update(Seq(b), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0))
    m.update(Nil, Set.empty, _ => true)
    (m.buildings, m.target) mustEqual (Vector.empty, None)
  }
  def rebuilt = {
    val m = new EnemyCampaignMemory
    m.update(Seq(building(1, 30)), Set.empty, _ => true)
    m.update(Nil, Set(1), _ => false)
    val rebuilt = building(2, 30)
    m.update(Seq(rebuilt), Set(1), _ => true)
    m.select(MapTilePosition(0, 0)) mustEqual Some(rebuilt.tile)
  }
  def freshMatch = {
    val old = new EnemyCampaignMemory
    old.update(Seq(building(1, 30)), Set.empty, _ => true)
    old.select(MapTilePosition(0, 0))
    val fresh = new EnemyCampaignMemory
    (fresh.buildings, fresh.target) mustEqual (Vector.empty, None)
  }
  def stableTargets = {
    val m = new EnemyCampaignMemory
    m.update(Seq(building(3, 1, false), building(2, 40), building(1, 30)), Set.empty, _ => true)
    m.select(MapTilePosition(0, 0)) mustEqual Some(MapTilePosition(30, 20))
  }
  def initializationOrder = {
    val uninitialized = Proxy.newProxyInstance(classOf[Universe].getClassLoader, Array[Class[_]](classOf[Universe]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef =
          throw new IllegalStateException("World dependency accessed before initialization: " + method.getName)
      }).asInstanceOf[Universe]
    new Strategy.Strategies(uninitialized).current.name mustEqual "Idle"
  }
  def nativeClock = {
    val c = new NativeFrameClock
    (c.advance(0), c.advance(0), c.advance(0), c.advance(1), c.advance(1), c.advance(0),
      new NativeFrameClock().advance(0)) mustEqual (true, false, false, true, false, false, true)
  }
  def builderTravel = {
    val travel = new ConstructionTravelProgress(0, MapTilePosition(0, 0))
    (travel.failed(61, MapTilePosition(1, 0), false, false, 60),
      travel.failed(1200, MapTilePosition(50, 0), false, false, 60),
      travel.failed(1921, MapTilePosition(50, 0), false, false, 60),
      travel.failed(1922, MapTilePosition(60, 0), true, false, 60),
      travel.failed(1983, MapTilePosition(60, 0), true, true, 60),
      travel.failed(1984, MapTilePosition(60, 0), true, false, 60)) mustEqual
      (false, false, true, false, false, true)
  }
  def staleGroup = {
    val units = AllUnits(new Units(null, false, null), new Units(null, true, null))
    val group = new Group[WrapsUnit](new Grid2D(10, 10, collection.immutable.BitSet.empty), units)
    (1 to 3).foreach(id => group.add_!(id -> MapTilePosition(id, 0)))
    (group.size, group.survivingMembers) mustEqual (3, Vector.empty)
  }
  def partialGroup = {
    val survivor = Proxy.newProxyInstance(classOf[WrapsUnit].getClassLoader, Array[Class[_]](classOf[WrapsUnit]),
      new InvocationHandler {
        override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef =
          if (method.getName == "nativeUnitId") Int.box(2) else throw new UnsupportedOperationException(method.getName)
      }).asInstanceOf[WrapsUnit]
    val own = new Units(null, false, null) { override def byId(id: Int) = if (id == 2) Some(survivor) else None }
    val group = new Group[WrapsUnit](new Grid2D(10, 10, collection.immutable.BitSet.empty),
      AllUnits(own, new Units(null, true, null)))
    (1 to 3).foreach(id => group.add_!(id -> MapTilePosition(id, 0)))
    group.survivingMembers.map(_.nativeUnitId) mustEqual Vector(2)
  }
  def singletonScout = {
    val a = MapTilePosition(1, 1)
    val b = MapTilePosition(2, 2)
    def next(points: Vector[MapTilePosition], covered: Set[MapTilePosition]) =
      ScoutPointPairs.next(points, covered)((_, _) => Some(1.0))
    (next(Vector(a), Set.empty), next(Vector(a), Set(a)), next(Vector(a, b), Set(a))) mustEqual
      (List(a), Nil, List(b))
  }
  def economicGate = {
    val c = TerranCampaignConfig()
    (c.ready(false, 12, 1500, 300, 1000, 300), c.ready(true, 12, 1500, 300, 999, 300),
      c.ready(true, 12, 1500, 300, 1000, 299), c.ready(true, 11, 1500, 300, 1000, 300),
      c.ready(true, 12, 1500, 300, 1000, 300)) mustEqual (false, false, false, false, true)
  }
  def saturation = {
    val progress = new TerranEconomicProgress
    progress.observe(Some(1), Seq(MiningFieldStatus(1, 20, 19, 10, true)))
    val early = progress.startingFieldSaturated
    progress.observe(Some(1), Seq(MiningFieldStatus(1, 20, 20, 10, true)))
    progress.observe(Some(1), Seq(MiningFieldStatus(1, 20, 19, 10, true)))
    (early, progress.startingFieldSaturated, new TerranEconomicProgress().startingFieldSaturated) mustEqual
      (false, true, false)
  }
  def secondField = {
    val p = new TerranEconomicProgress
    val home = MiningFieldStatus(1, 20, 20, 5, true)
    p.observe(Some(1), Seq(home))
    val flying = MiningFieldStatus(2, 20, 20, 5, false)
    val unworked = MiningFieldStatus(2, 20, 10, 0, true)
    def observe(fields: Seq[MiningFieldStatus]) = { p.observe(Some(1), fields); p.secondBaseEstablished }
    (observe(Seq(home, home)), observe(Seq(home, flying)), observe(Seq(home, unworked)),
      observe(Seq(home, unworked.copy(working = 1)))) mustEqual (false, false, false, true)
  }
  def relocation = {
    import DepotRelocation._
    (next(false, false, false, false, false), next(true, true, false, false, false),
      next(true, false, false, false, false), next(true, false, true, false, false),
      next(true, false, true, true, false), next(true, false, false, true, true),
      next(false, false, true, false, false)) mustEqual
      (AwaitSaturation, FinishTraining, Lift, Fly, Land, Established, Fly)
  }
  def workerQuota = {
    def missing(incomplete: Int, reserved: Int, native: Int, requests: Seq[Int]) =
      WorkerProductionQuota.missing(24, 20, incomplete, reserved, native, requests)
    (missing(0, 2, 0, Nil), missing(2, 2, 2, Nil), missing(0, 0, 2, Nil),
      missing(0, 0, 0, Seq(3)), missing(2, 2, 2, Seq(3))) mustEqual (2, 2, 2, 1, 0)
  }
  def localMining = {
    (LocalMineralMining.observed(false, 11, Some(11), true),
      LocalMineralMining.observed(true, 11, Some(9), true),
      LocalMineralMining.observed(true, 11, None, true),
      LocalMineralMining.observed(true, 11, Some(11), false),
      LocalMineralMining.observed(true, 11, Some(11), true)) mustEqual (false, false, false, false, true)
  }
  def stockpile = {
    val c = TerranCampaignConfig()
    (c.holdNewArmy(false, 12, 1500, 300, false), c.holdNewArmy(true, 11, 1500, 300, false),
      c.holdNewArmy(true, 12, 1500, 300, false), c.holdNewArmy(true, 12, 1500, 300, true)) mustEqual
      (false, false, true, false)
  }
  def exhaustedStart = {
    val p = new TerranEconomicProgress
    val home = MiningFieldStatus(1, 20, 20, 5, true)
    val next = MiningFieldStatus(2, 14, 5, 1, true)
    p.observe(Some(1), Seq(home))
    p.observe(Some(1), Seq(home, next))
    p.observe(Some(1), Seq(home.copy(capacity = 0, assigned = 0, working = 0), next))
    val c = TerranCampaignConfig()
    (p.startingFieldSaturated, p.secondBaseEstablished,
      c.ready(p.secondBaseEstablished, 30, 3000, 1000, 1000, 300),
      new TerranEconomicProgress().secondBaseEstablished) mustEqual (true, true, true, false)
  }
  def coveredStaffing = {
    // A relocated depot serves field2; the old field and distant field3 must not enqueue workers.
    val requests = Seq(1 -> 20, 2 -> 14, 3 -> 60)
    def demand(default: Boolean, landed: Option[Int]) = requests.filter { case (field, _) =>
      MineralFieldStaffing.permitted(default, landed, field)
    }.map(_._2).sum
    (demand(true, Some(2)), demand(true, None), demand(false, Some(2)),
      WorkerProductionQuota.missing(demand(true, Some(2)) + 6, 24, 0, 0, 0, Nil)) mustEqual
      (14, 0, 94, 0)
  }
  def depotPlacement = {
    val blocked = new Grid2D(128, 128, collection.immutable.BitSet.empty).mutableCopy
    blocked.block_!(Area(MapTilePosition(71, 115), Size(2, 1)).growBy(3))
    val refusedNativeSite = Area(MapTilePosition(69, 110), Size(4, 3))
    val clearHomeSite = Area(MapTilePosition(55, 108), Size(4, 3))
    (ResourceDepotPlacement.permitted(refusedNativeSite, true, blocked),
      ResourceDepotPlacement.permitted(clearHomeSite, true, blocked),
      ResourceDepotPlacement.permitted(refusedNativeSite, false, blocked)) mustEqual (false, true, true)
  }
  def refusedPlacement = {
    val refused = new ConstructionTravelProgress(8323, MapTilePosition(70, 115))
    val building = new ConstructionTravelProgress(8323, MapTilePosition(69, 110))
    // PlaceBuilding is an attempted command, not native ConstructingBuilding.
    (refused.failed(8324, MapTilePosition(70, 115), false, false, 60),
      refused.failed(9044, MapTilePosition(70, 115), false, false, 60),
      building.failed(8324, MapTilePosition(69, 110), true, true, 60),
      building.failed(12000, MapTilePosition(69, 110), true, true, 60)) mustEqual (false, true, false, false)
  }
  def terminalVision = {
    val fair = new NativeVisionCoverage
    val live = fair.observe(100, true, false, false)
    val terminal = fair.observe(101, true, true, true)
    val invalid = new NativeVisionCoverage
    invalid.observe(100, true, false, true) // No native victory/defeat proof: this is real invalid coverage.
    invalid.observe(101, true, true, false)
    (live, terminal, fair.liveSamples, fair.lastLiveFrame, fair.terminalSamples,
      fair.terminalCompleteMap, fair.ordinaryVision, invalid.ordinaryVision,
      new NativeVisionCoverage().ordinaryVision) mustEqual
      (true, false, 1, 100, 1, true, true, false, false)
  }
}
