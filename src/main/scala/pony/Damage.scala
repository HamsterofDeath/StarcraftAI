package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

case class Damage(baseAmount: Int, bonus: Int, cooldown: Int, damageType: DamageType, hitCount: Int,
                  upgradeLevel: Int, isAir: Boolean) {
  def damageIfHits(other: Armor, assumeHP: Int, assumeShields: Int, shotCount: Int) = {
    val maxDamage = upgradeLevel * bonus + baseAmount
    // TODO exact damage calculation is still unknown

    def calculate(against: Armor, assumeHP: Int, assumeShields: Int) = {
      val shield = assumeShields
      val subtractFromShields = {
        if (shield > 0) {
          assumeHP min maxDamage
        } else 0
      }

      val damageToHP = {
        val remainingDamage = maxDamage - subtractFromShields
        if (remainingDamage > 0) {
          val armorDivisor = against.armorType.damageFactorIfHitBy(damageType)
          val armor = against.armor
          val afterArmor = (remainingDamage - armor) max 1
          val afterFactor = (afterArmor * armorDivisor.factor) / 100
          afterFactor
        } else 0
      }
      DamageSingleAttack(damageToHP, subtractFromShields, isAir)
    }

    var shots = shotCount
    var hp = assumeHP
    var shields = assumeShields
    var hpDamage = 0
    var shieldDamage = 0
    while (shots > 0) {
      shots -= 1

      val damageOfFirstHit = calculate(other, hp, shields)
      val afterShot = {
        if (hitCount == 2) {
          val hpAfterFirstHit = hp - damageOfFirstHit.onHp
          val shieldAfterFirstHit = shields - damageOfFirstHit.onShields
          val damageOfSecondHit = calculate(other, hpAfterFirstHit, shieldAfterFirstHit)
          DamageSingleAttack(damageOfFirstHit.onHp + damageOfSecondHit.onHp,
            damageOfFirstHit.onShields + damageOfSecondHit.onShields, isAir)
        } else {
          assert(hitCount == 1)
          damageOfFirstHit
        }
      }

      hp -= afterShot.onHp
      shields -= afterShot.onShields
      hpDamage += afterShot.onHp
      shieldDamage += afterShot.onShields

    }

    DamageSingleAttack(hpDamage, shieldDamage, isAir)
  }
}
