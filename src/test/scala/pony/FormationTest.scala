package pony

import pony.geometry.MapTilePosition

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.pathing.Formation._

class FormationTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A route is measured along its corners and held to its ends $route
       |The longest reach leads, the rest stand back by what they lack, side by side in rows of seven $leaders
       |A slot lies behind the front by its lag, to the left of the march when positive $slots
       |A member has caught up near its slot or once level with it along the route $caught
       |The front moves on once four in five have caught up $keepingUp
       |A taut route goes straight to the furthest point in sight $tautRoute
       |The route is planned once half the army stands far enough from home $leaving
       |A front forms with the army's median member halfway down the formation $forming
       """.stripMargin

  private val corner = new Route(Seq(MapTilePosition(0, 0), MapTilePosition(10, 0), MapTilePosition(10, 10)))

  def route =
    (corner.length === 20.0) and
      (corner.at(5) === ((5.0, 0.0), (1.0, 0.0))) and
      (corner.at(15) === ((10.0, 5.0), (0.0, 1.0))) and
      (corner.at(-3)._1 === ((0.0, 0.0))) and
      (corner.at(25)._1 === ((10.0, 10.0))) and
      (corner.progressOf(MapTilePosition(7, 2)) === 7.0) and
      (corner.progressOf(MapTilePosition(12, 6)) === 16.0) and
      (new Route(Seq(MapTilePosition(3, 4))).at(2)._1 === ((3.0, 4.0)))

  def leaders = {
    val tank     = 1 -> 7.0
    val goliath  = 2 -> 6.0
    val firebat  = 3 -> 1.0
    val marines  = (10 until 19).map(_ -> 5.0)
    val placed   = places(Seq(tank, goliath, firebat) ++ marines)
    val marineAt = marines.map((id, _) => placed(id)).toList
    (placed(1) === ((0.0, 0.0))) and
      (placed(2) === ((1.0, 0.0))) and
      (placed(3) === ((6.0, 0.0))) and
      (marineAt.take(7) ===
        List((2.0, 0.0), (2.0, -1.0), (2.0, 1.0), (2.0, -2.0), (2.0, 2.0), (2.0, -3.0), (2.0, 3.0))) and
      (marineAt.drop(7) === List((3.5, 0.0), (3.5, -1.0))) and
      (depth(placed) === 6.0)
  }

  def slots = {
    val east = new Route(Seq(MapTilePosition(0, 0), MapTilePosition(20, 0)))
    (slot(east, 10, (2.0, 1.0)) === ((8.0, -1.0))) and
      (slot(east, 10, (0.0, -2.0)) === ((10.0, 2.0))) and
      (slot(east, 1, (6.0, 0.0)) === ((0.0, 0.0)))
  }

  def caught = (caughtUp(progress = 2, slotProgress = 10, distance = 3) must beTrue) and
    (caughtUp(progress = 7, slotProgress = 10, distance = 8) must beTrue) and
    (caughtUp(progress = 15, slotProgress = 10, distance = 8) must beTrue) and
    (caughtUp(progress = 6.5, slotProgress = 10, distance = 8) must beFalse)

  def keepingUp = (keepsUp(Seq.fill(8)(true) ++ Seq.fill(2)(false)) must beTrue) and
    (keepsUp(Seq.fill(7)(true) ++ Seq.fill(3)(false)) must beFalse) and
    (keepsUp(Nil) must beFalse)

  def tautRoute = {
    // in sight along a row or a column only
    def sees(a: MapTilePosition, b: MapTilePosition) = a.x == b.x || a.y == b.y
    def tile(x: Int, y: Int)                         = MapTilePosition(x, y)
    (taut(Seq(tile(0, 0), tile(4, 0), tile(4, 4), tile(4, 0), tile(8, 0)), sees) === Vector(tile(0, 0), tile(8, 0))) and
      (taut(Seq(tile(0, 0), tile(4, 0), tile(8, 0), tile(8, 4), tile(8, 8), tile(4, 8)), sees) ===
        Vector(tile(0, 0), tile(8, 0), tile(8, 8), tile(4, 8))) and
      (taut(Seq(tile(0, 0), tile(3, 3), tile(5, 5)), (_, _) => false) ===
        Vector(tile(0, 0), tile(3, 3), tile(5, 5))) and
      (taut(Nil, sees) === Vector.empty)
  }

  def leaving = (leftHome(Seq(5.0, 25.0, 30.0), 20) must beTrue) and
    (leftHome(Seq(5.0, 10.0, 30.0), 20) must beFalse) and
    (leftHome(Seq(5.0, 25.0), 20) must beTrue) and
    (leftHome(Nil, 20) must beFalse)

  def forming = (formingFront(Seq(0.0, 1.0, 2.0, 10.0, 11.0), 6.0) === 5.0) and (formingFront(Nil, 6.0) === 6.0)
}
