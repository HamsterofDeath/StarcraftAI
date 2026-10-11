package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.combat.BattleOdds._

import bwapi.UnitType
import bwapi.UnitType._

class BattleOddsTest extends Specification with MustMatchers {

  def is =
    s2"""
       |The odds are the square law scripts/battery_fit.py learned from the battery $matchesTheBattery
       |A matchup the battery never saw falls back to the stats $statsFallback
       |More enemies or hurt fighters lower the chance; an empty side wins or loses outright $monotone
       |The margin is positive exactly when the chance is above one half $marginSign
       """.stripMargin

  private def side(units: (UnitType, Int)*) = units.flatMap((t, n) => Seq.fill(n)(t -> 1.0))

  // P(win) of the learned square law in scripts/battery_fit.py for the same armies
  def matchesTheBattery =
    (winChance(side(Terran_Marine -> 20, Terran_Medic -> 4), side(Protoss_Dragoon -> 6, Protoss_Zealot -> 4)) must
      beCloseTo(0.7259, 0.001)) and
      (winChance(side(Terran_Battlecruiser -> 3), side(Protoss_Dragoon -> 6)) must beCloseTo(0.8772, 0.001)) and
      (winChance(side(Terran_Battlecruiser -> 3), side(Protoss_Dragoon -> 8)) must beCloseTo(0.4110, 0.001)) and
      (winChance(side(Terran_Battlecruiser -> 6), side(Protoss_Corsair -> 4, Protoss_Scout -> 3)) must
        beCloseTo(0.9939, 0.001)) and
      (winChance(
        side(Terran_Siege_Tank_Tank_Mode -> 4, Terran_Goliath -> 6, Terran_Marine -> 12),
        side(Protoss_Dragoon -> 8, Protoss_Zealot -> 6, Protoss_Archon -> 1)
      ) must beCloseTo(0.6292, 0.001))

  def statsFallback =
    (dps(Protoss_Photon_Cannon, Terran_Battlecruiser) must beCloseTo(17.0 * 24 / 22, 1e-9)) and
      (dps(Protoss_Carrier, Terran_Battlecruiser) must beCloseTo(8 * 3.0 * 24 / 30, 1e-9)) and
      (dps(Protoss_Reaver, Terran_Marine) must beCloseTo(40.0, 1e-9)) and
      (dps(Protoss_Reaver, Terran_Battlecruiser) === 0.0) and
      (dps(Protoss_Interceptor, Terran_Marine) === 0.0) and
      (durability(Protoss_Photon_Cannon) === 200.0) and
      (durability(Protoss_Interceptor) === 0.0) and
      // the stats behind the learned table: a firebat's two flames against a zealot's armor
      (statsDps(Terran_Firebat, Protoss_Zealot) must beCloseTo(7.0 * 2 * 24 / 22, 1e-9)) and
      (canHurt(Protoss_Zealot, Seq(Terran_Battlecruiser)) must beFalse) and
      (canHurt(Protoss_Photon_Cannon, Seq(Terran_Battlecruiser)) must beTrue)

  def monotone = {
    val cruisers = side(Terran_Battlecruiser -> 3)
    val hurt     = cruisers.map((t, _) => t -> 0.5)
    (winChance(cruisers, side(Protoss_Dragoon -> 5)) must beGreaterThan(winChance(cruisers, side(Protoss_Dragoon -> 6)))) and
      (winChance(hurt, side(Protoss_Dragoon -> 5)) must beLessThan(winChance(cruisers, side(Protoss_Dragoon -> 5)))) and
      (winChance(cruisers, Nil) === 1.0) and
      (winChance(Nil, side(Protoss_Zealot -> 1)) === 0.0) and
      (winChance(cruisers, side(Protoss_Interceptor -> 8)) === 1.0)
  }

  def marginSign = {
    val armies = Seq(
      side(Terran_Marine -> 20, Terran_Medic -> 4) -> side(Protoss_Dragoon -> 6, Protoss_Zealot -> 4),
      side(Terran_Battlecruiser -> 3) -> side(Protoss_Dragoon -> 8),
      side(Terran_Vulture -> 6) -> side(Protoss_Zealot -> 6)
    )
    armies.map((us, them) => (margin(us, them) > 0) === (winChance(us, them) > 0.5)).reduce(_ and _)
  }
}
