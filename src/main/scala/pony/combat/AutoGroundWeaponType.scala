package pony
package combat

trait AutoGroundWeaponType extends GroundWeapon {
  override val groundDamageType = DamageTypes.fromNative(groundWeapon.damageType)
}
