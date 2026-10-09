package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.KitingPolicy._

class KitingPolicyTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Without melee threats the unit is free for other behaviours $free
       |A shot under way is never interrupted $firing
       |A ready weapon focuses the weakest enemy in range $weakestInRange
       |A ready weapon attacks the closest enemy when none is in range $closestOutOfRange
       |A reloading unit retreats from a threat that would catch it $retreats
       |A reloading unit holds when every threat is far enough away $holds
       |A retreat never goes into unwalkable ground $avoidsWalls
       |A retreat prefers open ground over a corner $avoidsCorners
       """.stripMargin

  private val open: Point => Boolean = p => p.x >= 0 && p.y >= 0 && p.x < 2048 && p.y < 2048

  /** A vulture-like shooter in the middle of an open map. */
  private def shooter(cooldown: Int = 0, firing: Boolean = false, at: Point = Point(1024, 1024)) =
    Shooter(at, range = 176, cooldown = cooldown, firing = firing, speed = 6.4)

  /** A zealot-like threat. */
  private def zealot(id: Int, x: Double, y: Double, durability: Int = 160) =
    Threat(id, Point(x, y), durability, reach = 36, speed = 4)

  def free = decide(shooter(), Nil, open) === Free

  def firing = decide(shooter(firing = true), Seq(zealot(1, 1100, 1024)), open) === Hold

  def weakestInRange = {
    val threats = Seq(zealot(1, 1100, 1024, 160), zealot(2, 1150, 1024, 40), zealot(3, 1500, 1024, 10))
    decide(shooter(), threats, open) === Shoot(2)
  }

  def closestOutOfRange = decide(shooter(), Seq(zealot(1, 1400, 1024), zealot(2, 1300, 1024)), open) === Shoot(2)

  def retreats = {
    val me     = shooter(cooldown = 20)
    val threat = zealot(1, 1124, 1024)
    decide(me, Seq(threat), open) match {
      case Retreat(to) => to.distanceTo(threat.at) must be_>(me.at.distanceTo(threat.at))
      case other       => other === Retreat(Point(0, 0))
    }
  }

  def holds = decide(shooter(cooldown = 5), Seq(zealot(1, 1300, 1024)), open) === Hold

  def avoidsWalls = {
    val wallToTheLeft: Point => Boolean = p => open(p) && p.x > 1000
    val me                              = shooter(cooldown = 20, at = Point(1010, 1024))
    decide(me, Seq(zealot(1, 1080, 1024)), wallToTheLeft) match {
      case Retreat(to) => (wallToTheLeft(to) must beTrue) and (to.x must be_>(1000.0))
      case other       => other === Retreat(Point(0, 0))
    }
  }

  def avoidsCorners = {
    // threat comes from the middle; straight away from it is the corner at (0, 0)
    val me = shooter(cooldown = 20, at = Point(120, 120))
    decide(me, Seq(zealot(1, 190, 190)), open) match {
      // straight away from the threat would end 73 px from the corner; open ground keeps clear of it
      case Retreat(to) => to.distanceTo(Point(0, 0)) must be_>(120.0)
      case other       => other === Retreat(Point(0, 0))
    }
  }
}
