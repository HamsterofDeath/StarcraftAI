package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait CanDie extends WrapsUnit with CanBeUnderStorm {
  self =>
  def isAttackable = true

  val armorType: ArmorType
  val price               = Price(nativeUnit.getType.mineralPrice(), nativeUnit.getType.gasPrice())
  private val maxHp       = nativeUnit.getType.maxHitPoints()
  private val maxShields  = nativeUnit.getType.maxShields()
  private val disabled    = oncePerTick { evalLocked }
  private val myHitPoints = oncePerTick {
    val hp = {
      if (age == 0) {
        // cloaked units start with 0/0, which makes the ai think the unit is dead
        HitPoints(maxHp, maxShields)
      } else {
        HitPoints(nativeUnit.getHitPoints, nativeUnit.getShields)
      }
    }
    val armorLevel = universe.upgrades.armorForUnitType(self)
    Armor(armorType, hp, armorLevel, self)
  }

  private var lastFrameHp = HitPoints(-1, -1)
  // obviously wrong, but that doesn't matter
  private var dead   = false
  def percentageHPOk = {
    hitPoints.sum.toDouble / (maxHp + maxShields)
  }

  private val myAttackedByMelee = oncePerTick {
    surroundings.closeEnemyGroundUnits.exists {
      case gw: GroundWeapon => gw.isMelee &&
        !gw.isHarmlessNow &&
        gw.currentTarget.contains(self) &&
        gw.currentTile.distanceToIsLess(self.currentTile, 2)
      case _ => false
    }
  }

  private val myAttackedByCloaked = oncePerTick {
    surroundings.closeEnemyUnits.exists {
      case cw0: Weapon if cw0.isInstanceOf[CanCloak] =>
        val cw = cw0.asInstanceOf[Weapon & CanCloak]
        cw.isCloaked &&
        !cw.isExposed &&
        !cw.isHarmlessNow &&
        cw.cooldownTimer > 0
      case _ => false
    }
  }

  def underAttackByMelee = myAttackedByMelee.get

  def underAttackByCloaked = myAttackedByCloaked.get

  def isDamaged = isInGame && (hitPoints.shield < maxShields || hitPoints.hitpoints < maxHp) &&
    !isBeingCreated

  def hitPoints = myHitPoints.get.hp

  override def isInGame: Boolean = super.isInGame && !isDead

  def isDead = dead || hitPoints.isDead

  override def isNonFighter = isUnArmed || super.isNonFighter

  def isUnArmed = !canDoDamage && !hasSpells

  def isHarmlessNow = isIncapacitated || isUnArmed

  def isIncapacitated = disabled.get

  def isBeingAttacked = hitPoints < lastFrameHp

  private var tookDamageInTick = 0

  def notifyDead_!(): Unit = {
    dead = true
  }

  def matchThis[X](ifMobile: Mobile => X, ifBuilding: Building => X) = this match {
    case m: Mobile   => ifMobile(m)
    case b: Building => ifBuilding(b)
    case x           => !!!(s"Check this $x")
  }

  override def onTick_!() = {
    super.onTick_!()
    if (isBeingAttacked) {
      tookDamageInTick = currentTick
    }
  }

  def hasBeenAttackedSince(ticks: Int) = currentTick - tookDamageInTick <= ticks

  def armor = myHitPoints.get

  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.register_!(() => {
      lastFrameHp = hitPoints
    })
  }

  private def evalLocked = nativeUnit.isLockedDown || nativeUnit.isStasised
}
