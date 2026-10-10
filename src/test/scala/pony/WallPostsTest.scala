package pony

import pony.geometry.MapTilePosition

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.wall.WallPosts._

class WallPostsTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Infantry stands close behind the wall, tanks far behind it, mines in front of it $depths
       |Tanks stand out of a ranged Dragoon's reach of the wall yet cover its far side $tankRange
       |Posts spread along the wall $spread
       """.stripMargin

  /** A wall along y = 10 from x = 40 to 46; the main lies south of it. */
  private val wall    = (40 to 46).map(x => MapTilePosition(x, 10))
  private val planned = layout(wall, MapTilePosition(43, 30))

  def depths = (planned.posts(Role.Infantry).head._2 must beCloseTo(13.0, 0.01)) and
    (planned.posts(Role.Tank).head._2 must beCloseTo(18.5, 0.01)) and
    (planned.approach.head._2 must beCloseTo(6.0, 0.01))

  // a Dragoon with its range upgrade reaches six tiles from the wall's far side (y = 9); a sieged tank twelve
  def tankRange = {
    val tankY = planned.posts(Role.Tank).head._2
    (tankY - 9 must beGreaterThan(6.0)) and (tankY - 7 must beLessThan(12.0))
  }

  def spread = planned.posts(Role.Infantry).map(_._1).distinct.size must beGreaterThan(4)
}
