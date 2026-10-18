package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.combat.IrradiateChoice._

class IrradiateChoiceTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A group burns for more than the costliest single target $groupFirst
       |With no group the high-value target wins over the cheap one $valueFallback
       |The burn never counts more damage than it deals or a unit has $capped
       |Mechanics and units already burning add nothing $immune
       |Our own units in the burn can veto the cast $collateral
       """.stripMargin

  private val radius = 64.0

  private def blip(
      id: Int,
      x: Double = 0,
      y: Double = 0,
      durability: Double = 160,
      price: Double = 100,
      enemy: Boolean = true,
      organic: Boolean = true,
      burning: Boolean = false
  ) = Blip(id, x, y, durability, durability, price, enemy, organic, burning)

  // three zealots inside one burn, an archon far away on its own
  private val zealots = Seq(blip(1), blip(2, x = 32), blip(3, x = -32))
  private val archon  = blip(4, x = 1000, durability = 360, price = 400)
  private val crowd   = zealots :+ archon

  def groupFirst =
    (score(zealots.head, crowd, radius) must beGreaterThan(score(archon, crowd, radius))) and
      (best(crowd, crowd, radius).exists((c, _) => Set(1, 2, 3).contains(c.id)) must beTrue)

  // a templar (80 to burn, 200 to kill) against a lone zealot (160 and 100), too far apart to share a burn
  private val templar = blip(5, durability = 80, price = 200)
  private val loner   = blip(6, x = 1000)

  def valueFallback = best(Seq(templar, loner), Seq(templar, loner), radius).map(_._1.id) === Some(5)

  def capped =
    // 250 of damage on 360 of archon, plus that share of its 400 price — never the archon's whole 360 or 400
    (score(archon, Seq(archon), radius) must beCloseTo(250.0 + 400.0 * 250.0 / 360.0, 6.significantFigures)) and
      // and never more health than a hurt unit has left: 40 of damage and a quarter of its price
      (score(blip(7, durability = 40).copy(maxDurability = 160), Seq(blip(7, durability = 40).copy(maxDurability = 160)), radius) must beCloseTo(65.0, 6.significantFigures))

  def immune = {
    val dragoon = blip(8, durability = 180, price = 175, organic = false)
    val burning = blip(9, x = 32, burning = true)
    val centre  = blip(10)
    score(centre, Seq(centre, dragoon, burning), radius) must beCloseTo(
      score(centre, Seq(centre), radius),
      6.significantFigures
    )
  }

  def collateral = {
    val centre  = blip(11)
    val marines = (12 to 15).map(i => blip(i, x = 16, durability = 40, price = 50, enemy = false))
    best(Seq(centre), centre +: marines, radius) must beNone
  }
}
