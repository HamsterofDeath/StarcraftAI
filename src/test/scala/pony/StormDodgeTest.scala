package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.StormDodge._

class StormDodgeTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A unit far from every storm stays $farAway
       |A unit next to a storm flees straight away from its centre $flees
       |A unit in the very centre still gets out $centre
       """.stripMargin

  def farAway = escape((500.0, 500.0), Seq((100.0, 100.0)), underStorm = false) must beNone

  def flees = escape((540.0, 500.0), Seq((500.0, 500.0)), underStorm = false) === Some((500.0 + Escape, 500.0))

  def centre = escape((500.0, 500.0), Seq((500.0, 500.0)), underStorm = true).map(p =>
    math.hypot(p._1 - 500, p._2 - 500)
  ) === Some(Escape)
}
