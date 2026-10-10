package pony
package combat

import pony.units.CanDie
import pony.util.LazyVal

import pony.brain._

trait GroundWeapon extends Weapon {

  def isInstantAttackGround = false
  def isMelee               = groundRangeTiles <= 2

  val groundRangePixels               = groundWeapon.maxRange()
  val groundRangeTiles                = groundRangePixels / tileSize
  private val groundRangeTilesSquared = groundRangeTiles * groundRangeTiles
  val groundCanAttackAir              = groundWeapon.targetsAir()
  val groundCanAttackGround           = groundWeapon.targetsGround()
  val groundDamageMultiplier          = groundWeapon.damageFactor()
  def isInstantEffectAttackGround     = damageDelayFactorGround == 0
  val groundDamageType: DamageType
  protected lazy val groundWeapon = initialNativeType.groundWeapon()
  private val damage              = LazyVal.from {
    // will be invalidated on upgrade
    evalDamage(groundWeapon, groundDamageType, groundDamageMultiplier, targetsAir = false)
  }
  private val myInGroundWeaponRange = oncePerTick {
    geoHelper.circle(centerTile, math.round(groundRangePixels.toDouble / 32).toInt)
  }

  def damageDelayFactorGround: Int

  override def weaponRangeRadius: Int = super.weaponRangeRadius max groundRangePixels

  override def assumeShotDelayOn(target: CanDie) = {
    if (canAttackIfNear(target)) {
      damageDelayFactorGround
    } else
      super.assumeShotDelayOn(target)
  }

  override def canAttackIfNear(other: CanDie) = {
    super.canAttackIfNear(other) || selfCanAttack(other)
  }

  def inGroundWeaponRange = myInGroundWeaponRange.get

  override def calculateDamageOn(
      other: Armor,
      assumeHP: Int,
      assumeShields: Int,
      shotCount: Int
  ) = {
    if (selfCanAttack(other.owner)) {
      damage.damageIfHits(other, assumeHP, assumeShields, shotCount)
    } else {
      super.calculateDamageOn(other, assumeHP, assumeShields, shotCount)
    }
  }

  private def selfCanAttack(other: CanDie) = {
    matchOn[Boolean](other)(
      _ => groundCanAttackAir,
      _ => groundCanAttackGround,
      b => if (b.isFloating) groundCanAttackAir else groundCanAttackGround
    )
  }

  private def quickRangeExclusion(other: CanDie): Boolean = {
    groundRangeTilesSquared + 6 < other.centerTile.distanceSquaredTo(this.centerTile)
  }

  override def isInWeaponRangeExact(other: CanDie) = {
    if (selfCanAttack(other) && !quickRangeExclusion(other))

      matchOn(other)(
        air => nativeUnit.isInWeaponRange(other.nativeUnit),
        ground => nativeUnit.isInWeaponRange(other.nativeUnit),
        building => nativeUnit.isInWeaponRange(other.nativeUnit)
      )
    else
      super.isInWeaponRangeExact(other)
  }
  override protected def onUniverseSet(universe: Universe): Unit = {
    super.onUniverseSet(universe)
    universe.upgrades.register_!(_ => damage.invalidate())
  }
}
