package pony

import pony.Upgrades.SinglePointMagicSpell

trait AreaSpellcasterBuilding
    extends Building with Controllable with HasSinglePointMagicSpell with HasMana {

  override def canCastNow(tech: SinglePointMagicSpell) = {
    def hasMana = tech.energyNeeded <= mana
    super.canCastNow(tech) && hasMana
  }
}
