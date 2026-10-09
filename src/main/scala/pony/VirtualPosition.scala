package pony

trait VirtualPosition extends WrapsUnit with Virtual {

  case class PositionSnapshot(where: MapTilePosition, where32: MapPosition)

  private var lastSeen = Option.empty[PositionSnapshot]

  override def currentTile = lastSeen.map(_.where)
    .filterNot(_ => myVisible)
    .getOrElse(super.currentTile)

  override def currentPosition = lastSeen.map(_.where32)
    .filterNot(_ => myVisible)
    .getOrElse(super.currentPosition)

  private val myVisible = oncePerTick {
    nativeUnit.isVisible
  }

  override def remember_!() = {
    super.remember_!()
    if (isEnemy) {
      lastSeen = PositionSnapshot(currentTile, currentPosition).toSome
    }
  }

  override def forget_!() = {
    super.forget_!()
    lastSeen = None
  }

}
