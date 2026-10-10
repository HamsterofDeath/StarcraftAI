package pony
package combat

case class DamageSingleAttack(onHp: Int, onShields: Int, airHit: Boolean) extends HasHpAndShields {
  override def hp: Int = onHp

  override def shields: Int = onShields
}
