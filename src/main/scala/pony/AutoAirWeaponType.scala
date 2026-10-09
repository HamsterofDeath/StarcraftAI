package pony

trait AutoAirWeaponType extends AirWeapon {
  override val airDamageType = DamageTypes.fromNative(airWeapon.damageType)
}
