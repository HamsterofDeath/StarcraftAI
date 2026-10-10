package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.combat.BlindChoice._

class BlindChoiceTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Workers, short-lived units and the dying are not worth a flare $notWorth
       |A detector is worth a flare even when it is no fighter $detector
       |Detectors come before any fighter $detectorsFirst
       |A ranged unit comes before a melee unit of the same price $rangedFirst
       |A healthy unit comes before a damaged one of its kind $healthyFirst
       """.stripMargin

  private val dragoon  = Candidate(false, false, false, 175, 1.0, ranged = true)
  private val zealot   = Candidate(false, false, false, 100, 1.0, ranged = false)
  private val observer = Candidate(true, false, false, 100, 1.0, ranged = false)

  def notWorth = (worth(dragoon) must beTrue) and
    (worth(zealot.copy(worker = true)) must beFalse) and
    (worth(zealot.copy(disposable = true)) must beFalse) and
    (worth(dragoon.copy(health = 0.2)) must beFalse)

  def detector = worth(observer) must beTrue

  def detectorsFirst = score(observer) must beGreaterThan(score(Candidate(false, false, false, 600, 1.0, true)))

  def rangedFirst = score(zealot.copy(ranged = true)) must beGreaterThan(score(zealot))

  def healthyFirst = score(dragoon) must beGreaterThan(score(dragoon.copy(health = 0.5)))
}
