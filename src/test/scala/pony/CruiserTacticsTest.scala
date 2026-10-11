package pony

import pony.geometry.MapTilePosition

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.cruisers.CruiserTactics._

import bwapi.UnitType.{Protoss_Dragoon, Terran_Battlecruiser}

class CruiserTacticsTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A cruiser leaves below 40 percent and returns only when mended to 90 $repairHysteresis
       |A raid needs three fit cruisers and two thirds of the fleet, and ends worn down or below two $raidStartAndEnd
       |The crew grows with the fleet from two to ten SCVs $crew
       |A big fleet attacks as one group of four fifths $bigFleet
       |A raid runs from anti-air it would beat only with less than the run chance, hurt raiders sooner $hitAndRun
       |A raid is gathered once every cruiser is near the centre $gathering
       |Only a cruiser ahead of the group waits for it $stragglers
       |Known expansions come first, then likely expansion sites, the main, a lone building, a start $targets
       |Among expansions the one without enemy army seen near it comes first, even when farther $whereTheArmyIsNot
       |A raid waits until it can stand the anti-air met at its target in the last four minutes $defence
       |A crew SCV gives a cruiser up once its hit points stop rising for six seconds $crewStall
       """.stripMargin

  def repairHysteresis = (needsRepair(0.39, repairing = false) must beTrue) and
    (needsRepair(0.5, repairing = false) must beFalse) and
    (needsRepair(0.8, repairing = true) must beTrue) and
    (needsRepair(0.9, repairing = true) must beFalse)

  def raidStartAndEnd =
    (startsRaid(2, 2) must beFalse) and
      (startsRaid(3, 3) must beTrue) and
      (startsRaid(3, 8) must beFalse) and
      (startsRaid(6, 8) must beTrue) and
      (startsRaid(11, 14) must beTrue) and
      (startsRaid(8, 14) must beFalse) and
      (endsRaid(Seq(1.0)) must beTrue) and
      (endsRaid(Seq(0.5, 0.55)) must beTrue) and
      (endsRaid(Seq(0.6, 0.7)) must beFalse)

  def crew = (crewSize(0), crewSize(1), crewSize(4), crewSize(30)) === (0, 2, 4, 10)

  def bigFleet = (startsRaid(7, 12) must beFalse) and (startsRaid(10, 12) must beTrue) and
    (startsRaid(6, 9) must beFalse) and
    (startsRaid(8, 9) must beTrue)

  private def cruisers(n: Int, health: Double = 1.0) = Seq.fill(n)(Terran_Battlecruiser -> health)
  private def dragoons(n: Int)                       = Seq.fill(n)(Protoss_Dragoon -> 1.0)

  def hitAndRun = (outnumbered(cruisers(3), dragoons(6)) must beFalse) and
    (outnumbered(cruisers(3), dragoons(7)) must beTrue) and
    (outnumbered(cruisers(3, health = 0.7), dragoons(6)) must beTrue) and
    (outnumbered(cruisers(9), dragoons(18)) must beFalse) and
    (outnumbered(cruisers(3), Nil) must beFalse)

  def gathering = {
    val centre = MapTilePosition(50, 50)
    (gathered(Seq(MapTilePosition(48, 50), MapTilePosition(53, 52)), centre) must beTrue) and
      (gathered(Seq(MapTilePosition(48, 50), MapTilePosition(60, 50)), centre) must beFalse)
  }

  def stragglers = {
    val centre = MapTilePosition(50, 50)
    val target = MapTilePosition(90, 50)
    (waitsForGroup(MapTilePosition(62, 50), centre, target) must beTrue) and
      (waitsForGroup(MapTilePosition(38, 50), centre, target) must beFalse) and
      (waitsForGroup(MapTilePosition(54, 50), centre, target) must beFalse)
  }

  def targets = {
    val home      = MapTilePosition(30, 7)
    val enemyMain = MapTilePosition(64, 118)
    val natural   = MapTilePosition(76, 116)
    val pylon     = MapTilePosition(50, 60)
    val site      = MapTilePosition(90, 70)
    (
      choose(Seq(enemyMain, natural), Seq(site), Seq(enemyMain, natural, pylon), Seq(enemyMain), home),
      choose(Seq(enemyMain), Seq(site), Seq(enemyMain, pylon), Seq(enemyMain), home),
      choose(Seq(enemyMain), Nil, Seq(enemyMain, pylon), Seq(enemyMain), home),
      choose(Nil, Nil, Seq(pylon), Seq(enemyMain), home),
      choose(Nil, Nil, Nil, Seq(enemyMain), home)
    ) === (Some(natural), Some(site), Some(enemyMain), Some(pylon), Some(enemyMain))
  }

  def crewStall = (crewStalled(1000 + CrewStallFrames, 1000) must beFalse) and
    (crewStalled(1001 + CrewStallFrames, 1000) must beTrue)

  def defence =
    (remembered(Some((1500, 1000)), 1000 + DefenceMemory - 1) === Some(1500)) and
      (remembered(Some((1500, 1000)), 1000 + DefenceMemory) === None) and
      (strongEnough(cruisers(3), Seq.fill(5)(Protoss_Dragoon)) must beTrue) and
      (strongEnough(cruisers(3), Seq.fill(6)(Protoss_Dragoon)) must beFalse) and
      (strongEnough(cruisers(4), Seq.fill(6)(Protoss_Dragoon)) must beTrue) and
      (strongEnough(cruisers(3), Nil) must beTrue)

  def whereTheArmyIsNot = {
    val home   = MapTilePosition(30, 7)
    val near   = MapTilePosition(40, 60)
    val far    = MapTilePosition(80, 100)
    val start  = MapTilePosition(64, 118)
    val guards = (t: MapTilePosition) => if (t == near) 1200.0 else 0.0
    (choose(Seq(near, far), Nil, Seq(near, far), Seq(start), home) === Some(near)) and
      (choose(Seq(near, far), Nil, Seq(near, far), Seq(start), home, guards) === Some(far))
  }
}
