package pony

import org.specs2.Specification
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
}
