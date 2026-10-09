package pony

import pony.Upgrades.SinglePointMagicSpell

trait HasSinglePointMagicSpell extends WrapsUnit {

  type Caster <: HasSinglePointMagicSpell
  val spells: List[SinglePointMagicSpell]
  protected val cooldown                                            = 24
  private var lastCast                                              = -9999
  override def hasSpells                                            = true
  def toOrder(tech: SinglePointMagicSpell, target: MapTilePosition) = {
    assert(canCastNow(tech))
    lastCast = universe.currentTick
    Orders.TechOnTile(this, target, tech)
  }

  def canCastNow(tech: SinglePointMagicSpell) = {
    assert(spells.contains(tech))
    def isReadyForCastCool = lastCast + cooldown < universe.currentTick
    isReadyForCastCool
  }

}
