package pony

import bwapi.{DamageType => _, _}

trait Weapon extends Controllable with ArmedUnit {
  self: WrapsUnit =>

  private val myTarget = oncePerTick {
    nativeUnit.getTarget != null && nativeUnit.getOrderTarget != null
  }

  def hasTarget = myTarget.get

  def assumeShotDelayOn(target: CanDie): Int = !!!("This should never be called")

  override def canDoDamage = true

  def isReadyToFireWeapon = cooldownTimer == 0

  def isAttacking = isStartingToAttack || cooldownTimer > 0

  def cooldownTimer = nativeUnit.getAirWeaponCooldown max nativeUnit.getGroundWeaponCooldown

  def isStartingToAttack = nativeUnit.isStartingAttack
  // air & groundweapon need to override this
  def canAttackIfNear(other: CanDie) = false
  def calculateDamageOn(
      other: CanDie,
      assumeHP: Int,
      assumeShields: Int,
      shotCount: Int
  ): DamageSingleAttack = {
    calculateDamageOn(other.armor, assumeHP, assumeShields, shotCount)
  }
  def calculateDamageOn(
      other: Armor,
      assumeHP: Int,
      assumeShields: Int,
      shotCount: Int
  ): DamageSingleAttack = {
    !!!("Forgot to override this")
  }

  def matchOn[X](other: CanDie)(ifAir: AirUnit => X, ifGround: GroundUnit => X, ifBuilding: Building => X) =
    other match {
      case a: AirUnit    => ifAir(a)
      case g: GroundUnit => ifGround(g)
      case b: Building   => ifBuilding(b)
      case x             => !!!(s"Check this $x")
    }
  // needs to be overridden
  def isInWeaponRangeExact(target: CanDie): Boolean = false

  def weaponRangeRadiusTiles = weaponRangeRadius / 32

  // needs to be overridden
  def weaponRangeRadius: Int = 0

  protected def evalDamage(
      weapon: WeaponType,
      damageType: DamageType,
      hitCount: Int,
      targetsAir: Boolean
  ) = {
    val level = universe.upgrades.weaponLevelOf(self)
    Damage(
      weapon.damageAmount(),
      weapon.damageBonus(),
      weapon.damageCooldown(),
      damageType,
      hitCount,
      level,
      targetsAir
    )
  }

}
