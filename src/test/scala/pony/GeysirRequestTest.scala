package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.economy.ManageMiningAtGeysirs._

class GeysirRequestTest extends Specification with MustMatchers {

  def is =
    s2"""
       |The first refinery request goes out at once, even at frame 0 $first
       |Another follows only a game minute after the last $retry
       """.stripMargin

  def first = (mayRequest(0, None) must beTrue) and (mayRequest(5000, None) must beTrue)

  def retry = (mayRequest(1000, Some(0)) must beFalse) and (mayRequest(RetryFrames, Some(0)) must beTrue)
}
