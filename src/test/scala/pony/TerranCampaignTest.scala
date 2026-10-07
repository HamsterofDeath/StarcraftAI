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
    (allowed(700), allowed(699), allowed(700, true), allowed(700, safe = false)) mustEqual
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
}
