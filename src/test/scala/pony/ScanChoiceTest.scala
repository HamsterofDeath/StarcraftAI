package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.ScanChoice._

class ScanChoiceTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Nothing is swept where nobody could shoot what the sweep reveals $noHunters
       |A hidden attacker comes before unexplained damage, which comes before a hidden detector $order
       |A spot inside a sweep still running is not swept again $noRepeat
       |Among equals the spot with more hunters is swept $moreHunters
       """.stripMargin

  private val here  = MapTilePosition(40, 40)
  private val there = MapTilePosition(80, 80)

  def noHunters = choose(Seq(Candidate(here, Reason.HiddenAttacker, 0, "DT")), Nil) must beNone

  def order = {
    val detector = Candidate(here, Reason.HiddenDetector, 5, "Observer")
    val damage   = Candidate(there, Reason.UnseenDamage, 1, "SCV")
    val attacker = Candidate(MapTilePosition(10, 10), Reason.HiddenAttacker, 1, "DT")
    (choose(Seq(detector, damage, attacker), Nil).map(_.reason) === Some(Reason.HiddenAttacker)) and
      (choose(Seq(detector, damage), Nil).map(_.reason) === Some(Reason.UnseenDamage))
  }

  def noRepeat =
    (choose(Seq(Candidate(here, Reason.HiddenAttacker, 3, "DT")), Seq(MapTilePosition(43, 42))) must beNone) and
      (choose(Seq(Candidate(here, Reason.HiddenAttacker, 3, "DT")), Seq(there)) must beSome)

  def moreHunters = choose(
    Seq(Candidate(here, Reason.UnseenDamage, 1, "SCV"), Candidate(there, Reason.UnseenDamage, 4, "Marine")),
    Nil
  ).map(_.at) === Some(there)
}
