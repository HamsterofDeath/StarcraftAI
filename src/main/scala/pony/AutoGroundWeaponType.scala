package pony

trait AutoGroundWeaponType extends GroundWeapon {
  override val groundDamageType = DamageTypes.fromNative(groundWeapon.damageType)
}
