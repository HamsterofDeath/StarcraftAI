package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.e2e.GenerateBattery._
import pony.e2e.GenerateE2EMaps.{Firebat, Marine, Medic}
import pony.e2e.chk.MapUnit

import scala.util.Random

class BatteryTest extends Specification with MustMatchers {

  def is =
    s2"""
       |The same seed draws the same battles $reproducible
       |Armies have one to three Terran or one to two Protoss types, each one to 24 strong $armies
       |Medics only come along with infantry $medics
       |Every unit starts on the 64x64 map, the bot's west of the computer's $placement
       |The manifest lists every battle with its armies and their values $manifestLists
       """.stripMargin

  private def battles(seed: Long) = {
    val random = new Random(seed)
    (1 to 200).map(sample(random, _))
  }

  def reproducible = battles(7) === battles(7)

  def armies = {
    val all = battles(1)
    (all.map(_.ours.size).toSet must beEqualTo(Set(1, 2, 3))) and (all.map(_.theirs.size).toSet === Set(1, 2)) and
      (all.forall(b => (b.ours ++ b.theirs).forall((_, n) => n >= 1 && n <= MaxPerType)) must beTrue) and
      (all.forall(_.ours.forall((s, _) => TerranPool.contains(s))) must beTrue) and
      (all.forall(_.theirs.forall((s, _) => ProtossPool.contains(s))) must beTrue)
  }

  def medics = battles(2).filter(_.ours.exists(_._1 == Medic)).forall(b =>
    b.ours.exists((s, _) => s == Marine || s == Firebat)
  ) must beTrue

  def placement = {
    val placed = battles(3).flatMap(b => scenario(b).units.filter(_.unitId != MapUnit.StartLocation))
    (placed.forall(u => u.x > 0 && u.x < 64 * 32 && u.y > 0 && u.y < 64 * 32) must beTrue) and
      (placed.filter(_.owner == 0).map(_.x).max must beLessThan(placed.filter(_.owner == 1).map(_.x).min))
  }

  def manifestLists = {
    val some = battles(4).take(3)
    val text = manifest(some)
    (some.forall(b => text.contains(s""""id":"${b.id}"""")) must beTrue) and (text must contain("\"oursValue\":")) and
      (text must contain("\"unit\":\"Terran_")) and (text must contain("\"unit\":\"Protoss_"))
  }
}
