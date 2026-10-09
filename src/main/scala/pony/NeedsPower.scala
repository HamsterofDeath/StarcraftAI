package pony

trait NeedsPower extends Building {
  private val myPowered = oncePerTick {
    nativeUnit.isPowered
  }
  def isPowered = myPowered.get

  override def isHarmlessNow = super.isHarmlessNow || !isPowered
}
