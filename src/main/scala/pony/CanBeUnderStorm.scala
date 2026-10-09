package pony

trait CanBeUnderStorm extends WrapsUnit {
  private val myUnder = oncePerTick {
    IsUnder(
      nativeUnit.isUnderAttack,
      nativeUnit.isUnderStorm,
      nativeUnit.isUnderDarkSwarm,
      nativeUnit.isUnderDisruptionWeb,
      currentTick
    )
  }

  case class LastKnownUnderPsi(where: MapTilePosition, when: Int)

  private var lastKnownUnderPsi = Option.empty[LastKnownUnderPsi]
  def isUnderPsiStorm           = myUnder.storm

  def wasUnderPsiStormSince(ticks: Int) = lastKnownUnderPsi.exists(_.when + ticks >= currentTick)

  def lastKnownStormPosition = lastKnownUnderPsi.map(_.where)
  override def onTick_!()    = {
    super.onTick_!()
    if (isUnderPsiStorm) {
      lastKnownUnderPsi = LastKnownUnderPsi(currentTile, currentTick).toSome
    }
  }
}
