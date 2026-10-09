package pony

trait CanUseStimpack extends Mobile with Weapon with HasSingleTargetSpells {
  private val stimmed  = oncePerTick { nativeUnit.isStimmed || stimTime > 0 }
  def isStimmed        = stimmed.get
  private def stimTime = nativeUnit.getStimTimer
}
