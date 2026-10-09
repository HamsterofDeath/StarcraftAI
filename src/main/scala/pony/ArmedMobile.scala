package pony

trait ArmedMobile extends Mobile with Weapon {
  def isInFight = {
    isStartingToAttack || isAttacking || isBeingAttacked
  }
}
