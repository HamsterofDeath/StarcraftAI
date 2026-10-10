package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.EnemyEstimate._

class EnemyEstimateTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A base earns nothing at its start, ramps up over six minutes, then earns its full rate $ramp
       |Buildings, workers and losses come off the bound, which never drops below zero $bound
       |More bases mean more income and more workers $moreBases
       """.stripMargin

  def ramp =
    (income(0, 1.0) === 0.0) and
      (income(RampFrames, 1.0) must beCloseTo(RampFrames * 0.6, 0.01)) and
      (income(RampFrames + 1000, 1.0) - income(RampFrames, 1.0) must beCloseTo(1000.0, 0.01))

  def bound = {
    val now  = RampFrames * 2
    val free = Model(now, Seq(0), 0.8, 0, 0).estimate
    val paid = Model(now, Seq(0), 0.8, 2000, 1500).estimate
    (free.upper - paid.upper must beCloseTo(3500.0, 0.01)) and
      (Model(100, Seq(0), 0.8, 5000, 0).estimate.upper === 0.0)
  }

  def moreBases = {
    val one = Model(RampFrames * 2, Seq(0), 0.8, 0, 0)
    val two = Model(RampFrames * 2, Seq(0, RampFrames), 0.8, 0, 0)
    (two.estimate.gathered must beGreaterThan(one.estimate.gathered)) and (two.workers must beGreaterThan(one.workers))
  }
}
