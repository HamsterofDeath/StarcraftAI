package pony

import pony.brain._

trait AirWeapon extends Weapon {

  def isInstantAttackAir = false
  val airRangePixels     = airWeapon.maxRange()
  // fails at goliath range upgrade
  val airRangeTiles                = airRangePixels / tileSize
  private val airRangeTilesSquared = airRangeTiles * airRangeTiles
  val airCanAttackAir              = airWeapon.targetsAir()
  val airCanAttackGround           = airWeapon.targetsGround()
  val airDamageMultiplier          = airWeapon.damageFactor()
  val airDamageType: DamageType
  def isInstantEffectAttackAir = damageDelayFactorAir == 0
  protected lazy val airWeapon = initialNativeType.airWeapon()
  private val damage           = LazyVal.from {
    // will be invalidated on upgrade
    evalDamage(airWeapon, airDamageType, airDamageMultiplier, targetsAir = true)
  }

  private val myInAirWeaponRange = oncePerTick {
    geoHelper.circle(centerTile, math.round(airRangePixels.toDouble / 32).toInt)
  }
  def inAirWeaponRange = myInAirWeaponRange.get

  def damageDelayFactorAir: Int
  override def weaponRangeRadius: Int            = super.weaponRangeRadius max airRangePixels
  override def assumeShotDelayOn(target: CanDie) = {
    if (canAttackIfNear(target)) {
      damageDelayFactorAir
    } else
      super.assumeShotDelayOn(target)
  }
  // air & groundweapon need to override this
  override def canAttackIfNear(other: CanDie) = {
    super.canAttackIfNear(other) || selfCanAttack(other)
  }
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
    matchOn(other)(
      _ => airCanAttackAir,
      _ => airCanAttackGround,
      b => if (b.isFloating) airCanAttackAir else airCanAttackGround
    )
  }

  private def quickRangeExclusion(other: CanDie): Boolean = {
    airRangeTilesSquared + 6 < other.centerTile.distanceSquaredTo(this.centerTile)
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
