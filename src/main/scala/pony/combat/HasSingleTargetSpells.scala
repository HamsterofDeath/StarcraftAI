package pony
package combat

import pony.units.{HasMana, Mobile}

import pony.tech.Upgrades.SingleTargetMagicSpell

trait HasSingleTargetSpells extends Mobile with HasMana {
  type CasterType <: HasSingleTargetSpells
  val spells: Seq[SingleTargetSpell[CasterType, ?]]
  protected val cooldown                                    = 24
  private var lastCast                                      = -9999
  override def hasSpells                                    = true
  def toOrder(tech: SingleTargetMagicSpell, target: Mobile) = {
    assert(canCastNow(tech))
    lastCast = universe.currentTick
    Orders.TechOnTarget(this, target, tech)
  }
  def canCastNow(tech: SingleTargetMagicSpell) = {
    assert(spells.exists(_.tech == tech))
    def hasMana            = tech.energyNeeded <= mana
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    hasMana && isReadyForCastCool
  }
}
