package pony

import org.specs2._

class MathTest extends Specification {

  def is =
    s2"""
       |Spiral should give any result $spiralResult
       |Single element spiral should work as expected $spiralSingle
       |9 element spiral should work as expected $spiral9
       |Bigger spiral should work as expected $spiralMany
                                 """.stripMargin
  def spiralResult = {
    testSpiral.isEmpty mustEqual false
  }
  def spiralSingle = {
    val first = testSpiral.head
    first mustEqual(100, 100)
  }
  private def testSpiral = new GeometryHelpers(200, 200).blockSpiralClockWise(MapTilePosition.shared(100, 100), 5)
                           .map(_.asTuple)
  def spiral9 = {
    val it = testSpiral

    val expected = List((100, 100), (101, 100), (101, 101), (100, 101), (99, 101), (99, 100), (99, 99), (100, 99),
      (101, 99), (102, 99))

    expected mustEqual it.take(expected.size)
  }
  def spiralMany = {
    val it = testSpiral

    val expected = List((100, 100), (101, 100), (101, 101), (100, 101), (99, 101), (99, 100), (99, 99), (100, 99),
      (101, 99), (102, 99), (102, 100), (102, 101), (102, 102), (101, 102))

    expected mustEqual it.take(expected.size)
  }
}