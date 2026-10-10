package pony

import pony.geometry.MapTilePosition

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.cruisers.CruiserTactics._

class CruiserTacticsTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A cruiser leaves below 40 percent and returns only when mended to 90 $repairHysteresis
       |A raid needs three fit cruisers and two thirds of the fleet, and ends worn down or below two $raidStartAndEnd
       |The crew grows with the fleet from two to ten SCVs $crew
       |A big fleet attacks as one group of four fifths $bigFleet
       |A raid runs from anti-air outweighing it, a big group only from a clearly stronger one $hitAndRun
       |A raid is gathered once every cruiser is near the centre $gathering
       |Only a cruiser ahead of the group waits for it $stragglers
       |Known expansions come first, then likely expansion sites, the main, a lone building, a start $targets
       |Among expansions the one without enemy army seen near it comes first, even when farther $whereTheArmyIsNot
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
      (endsRaid(Seq(1.0)) must beTrue) and
      (endsRaid(Seq(0.5, 0.55)) must beTrue) and
      (endsRaid(Seq(0.6, 0.7)) must beFalse)

  def crew = (crewSize(0), crewSize(1), crewSize(4), crewSize(30)) === (0, 2, 4, 10)

  def bigFleet = (startsRaid(7, 12) must beFalse) and (startsRaid(10, 12) must beTrue) and
    (startsRaid(6, 9) must beFalse) and
    (startsRaid(8, 9) must beTrue)

  def hitAndRun = (outnumbered(1000, 2100, bigGroup = false) must beFalse) and
    (outnumbered(1800, 2100, bigGroup = false) must beTrue) and
    (outnumbered(1800, 2100, bigGroup = true) must beFalse) and
    (outnumbered(3200, 2100, bigGroup = true) must beTrue)

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
