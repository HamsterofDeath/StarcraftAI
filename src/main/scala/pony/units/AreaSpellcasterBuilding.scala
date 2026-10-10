package pony
package units

import pony.combat.HasSinglePointMagicSpell

import pony.tech.Upgrades.SinglePointMagicSpell

trait AreaSpellcasterBuilding
    extends Building with Controllable with HasSinglePointMagicSpell with HasMana {

  override def canCastNow(tech: SinglePointMagicSpell) = {
    def hasMana = tech.energyNeeded <= mana
    super.canCastNow(tech) && hasMana
  }
}
