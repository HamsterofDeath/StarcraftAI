package pony

import org.specs2._
import org.specs2.matcher.MustMatchers

class MathTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Spiral should give any result $spiralResult
       |Single element spiral should work as expected $spiralSingle
       |9 element spiral should work as expected $spiral9
       |Bigger spiral should work as expected $spiralMany
       |Shared points should work as expected $sharedPoints
                                 """.stripMargin
  def spiralResult = {
    testSpiral.isEmpty === false
  }
  def spiralSingle = {
    val first = testSpiral.head
    first ===(100, 100)
  }
  private def testSpiral = new GeometryHelpers(200, 200).blockSpiralClockWise(MapTilePosition.shared(100, 100), 5)
                           .map(_.asTuple).toList
  def spiral9 = {
    val it = testSpiral

    val expected = List((100, 100), (101, 100), (101, 101), (100, 101), (99, 101), (99, 100), (99, 99), (100, 99),
      (101, 99), (102, 99))

    expected === it.take(expected.size)
  }
  def spiralMany = {
    val it = testSpiral

    val expected = List((100, 100), (101, 100), (101, 101), (100, 101), (99, 101), (99, 100), (99, 99), (100, 99),
      (101, 99), (102, 99), (102, 100), (102, 101), (102, 102), (101, 102))

    expected === it.take(expected.size)
  }

  def sharedPoints = {
    MapTilePosition.shared(0, 0) === MapTilePosition(0, 0)
  }
}