package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.wall.WallGeometry._

class WallGeometryTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Supply depot and barracks boxes match BWAPI $boxesMatchBwapi
       |Two stacked depots stop a zealot $stackedDepots
       |A barracks above a depot leaves a 25 pixel slit a zealot passes $barracksAboveDepot
       |A depot above a barracks leaves 13 pixels and stops a zealot $depotAboveBarracks
       |With the barracks lifted a siege tank passes the depot $liftedGate
       |Blocked walk tiles stop units like buildings $terrain
       |A trained Marine appears past the barracks' lower left corner $exitLowerLeft
       |A trained Marine appears beside that corner when a building stands on it $exitBlocked
       """.stripMargin

  /** A pass five tiles high; units start left of the wall at x = 128 and must reach x > 300. */
  private val region                      = Box(0, 0, 351, 159)
  private val outside                     = Seq((20, 40), (20, 80), (20, 120))
  private def inside(x: Int, y: Int)      = x > 300
  private val open: (Int, Int) => Boolean = (_, _) => false

  private def pass(unit: Dims, buildings: (Int, Int, Dims)*) =
    passable(unit, buildings.map((x, y, d) => buildingBox(x, y, d)), open, region, outside, inside)

  def boxesMatchBwapi = (Dims.of(bwapi.UnitType.Terran_Supply_Depot) === Dims.SupplyDepot) and
    (Dims.of(bwapi.UnitType.Terran_Barracks) === Dims.Barracks) and
    (Dims.of(bwapi.UnitType.Protoss_Zealot) === Dims.Zealot) and
    (Dims.of(bwapi.UnitType.Terran_Marine) === Dims.Marine)

  def stackedDepots =
    pass(Dims.Zealot, (4, 0, Dims.SupplyDepot), (4, 2, Dims.SupplyDepot), (4, 4, Dims.SupplyDepot)) must beFalse

  def barracksAboveDepot = pass(Dims.Zealot, (4, 0, Dims.Barracks), (4, 3, Dims.SupplyDepot)) must beTrue

  def depotAboveBarracks = pass(Dims.Zealot, (4, 0, Dims.SupplyDepot), (4, 2, Dims.Barracks)) must beFalse

  def liftedGate = pass(Dims.SiegeTank, (4, 0, Dims.SupplyDepot)) must beTrue

  def terrain = {
    // a solid column of blocked walk tiles across the whole pass
    val cliff: (Int, Int) => Boolean = (wx, _) => wx == 20
    passable(Dims.Marine, Nil, cliff, region, outside, inside) must beFalse
  }

  // a barracks at tile (4, 0): its box runs from x = 144 to 248 and down to y = 80
  private val barracks = buildingBox(4, 0, Dims.Barracks)

  // 144 - 8 - 1 = 135 and 80 + 9 + 1 = 90: the nearest center on the four pixel grid clear of the barracks
  def exitLowerLeft = exitCenter(Dims.Marine, barracks, Seq(barracks), open, region) === Some((136, 92))

  // a depot at tile (2, 2) covers x = 74 to 150 and y = 74 to 122: the Marine appears right of it, under the barracks
  def exitBlocked = {
    val depot = buildingBox(2, 2, Dims.SupplyDepot)
    exitCenter(Dims.Marine, barracks, Seq(barracks, depot), open, region) === Some((160, 92))
  }
}
