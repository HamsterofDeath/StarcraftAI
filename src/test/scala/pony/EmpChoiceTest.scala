package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.EmpChoice._

class EmpChoiceTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Energy counts three times as much as shields by default $energyWeighs
       |A blast that would drain our own casters loses their worth $ownLoss
       |A blast below the minimum is not worth the energy $tooLittle
       |Weights read as shields,energy $parse
       |An unseen caster's energy is estimated from when we first saw it, up to its maximum $energy
       """.stripMargin

  private val archon  = Blip(100, 100, 350, 0, enemy = true)
  private val templar = Blip(400, 100, 40, 150, enemy = true)
  private val vessel  = Blip(420, 100, 0, 200, enemy = false)

  def energyWeighs = best(Seq(archon, templar), Seq(archon, templar), 64, DefaultWeights).map(_._1) === Some(templar)

  def ownLoss = best(Seq(archon, templar), Seq(archon, templar, vessel), 64, DefaultWeights).map(_._1) === Some(archon)

  def tooLittle = best(Seq(templar), Seq(templar.copy(shields = 0, energy = 20)), 64, DefaultWeights) must beNone

  def parse = (parseWeights("1, 3") === Some((1.0, 3.0))) and (parseWeights("x") must beNone)

  def energy = (pony.brain.modules.EnemyEnergy.estimate(0, 200) === 50.0) and
    (pony.brain.modules.EnemyEnergy.estimate(100000, 200) === 200.0)
}
