package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.cruisers.YamatoChoice._

class YamatoChoiceTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A High Templar is shot before a Dragoon $casterFirst
       |A unit the shot kills is preferred over a sturdier one of the same value $killFirst
       |Workers and cheap units are not worth the energy $notWorth
       |Units that cannot shoot at air count for little $groundOnly
       """.stripMargin

  private val templar = Candidate(1, 200, 80, 0.5, hitsAir = false, caster = true)
  private val dragoon = Candidate(2, 175, 180, 1.0, hitsAir = true, caster = false)
  private val carrier = Candidate(3, 600, 450, 1.0, hitsAir = true, caster = false)
  private val probe   = Candidate(4, 50, 40, 0.5, hitsAir = false, caster = false)
  private val zealot  = Candidate(5, 100, 160, 0.5, hitsAir = false, caster = false)

  def casterFirst = best(Seq(dragoon, templar)).map(_.id) === Some(1)

  def killFirst = score(dragoon) must beGreaterThan(score(dragoon.copy(durability = 400)))

  def notWorth = best(Seq(probe)) must beNone

  def groundOnly = (score(zealot) must beLessThan(MinScore)) and (best(Seq(carrier)).map(_.id) === Some(3))
}
