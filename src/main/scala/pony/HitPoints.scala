package pony

case class HitPoints(hitpoints: Int, shield: Int) {

  def isDead = hitpoints == 0

  def sum = hitpoints + shield

  def <=(other: HitPoints): Boolean = <=(other.hitpoints, other.shield)

  def <=(otherHp: Int, otherShield: Int) = {
    hitpoints <= otherHp || shield <= otherShield
  }

  def <(other: HitPoints): Boolean = <(other.hitpoints, other.shield)

  def <(otherHp: Int, otherShield: Int) = {
    hitpoints < otherHp || shield < otherShield
  }

  def <(t: (Int, Int)) = {
    hitpoints < t._1 || shield < t._2
  }
}
