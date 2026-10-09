package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.CruiserTactics._

class CruiserTacticsTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A cruiser leaves below 40 percent and returns only when mended to 90 $repairHysteresis
       |A raid needs three fit cruisers and two thirds of the fleet, and ends worn down or below two $raidStartAndEnd
       |The crew grows with the fleet from two to six SCVs $crew
       |Known expansions come first, then likely expansion sites, the main, a lone building, a start $targets
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

  def crew = (crewSize(0), crewSize(1), crewSize(3), crewSize(30)) === (0, 2, 3, 6)

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
}
