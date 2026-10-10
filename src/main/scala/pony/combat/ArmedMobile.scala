package pony
package combat

import pony.units.Mobile

trait ArmedMobile extends Mobile with Weapon {
  def isInFight = {
    isStartingToAttack || isAttacking || isBeingAttacked
  }
}
