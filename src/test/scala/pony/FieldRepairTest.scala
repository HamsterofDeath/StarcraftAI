package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.FieldRepair._

class FieldRepairTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A raid whose centre stays put over ten seconds holds still $holds
       |A moving raid or a short trail does not $moves
       |The crew comes only with enough cruisers hurt and no enemy ground fighters near $wantedRule
       """.stripMargin

  private val here = MapTilePosition(50, 50)

  def holds = stationary(Seq(0 -> here, 120 -> MapTilePosition(51, 50), 240 -> MapTilePosition(52, 51)), 240) must beTrue

  def moves =
    (stationary(Seq(0 -> here, 120 -> MapTilePosition(56, 50), 240 -> MapTilePosition(60, 50)), 240) must beFalse) and
      (stationary(Seq(100 -> here, 240 -> here), 240) must beFalse)

  def wantedRule =
    (wanted(stationary = true, hurt = 2, enemyGroundNear = false) must beTrue) and
      (wanted(stationary = true, hurt = 1, enemyGroundNear = false) must beFalse) and
      (wanted(stationary = true, hurt = 3, enemyGroundNear = true) must beFalse) and
      (wanted(stationary = false, hurt = 3, enemyGroundNear = false) must beFalse)
}
