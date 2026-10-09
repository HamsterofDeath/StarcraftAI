package pony

trait Resource extends BlockingTiles {
  val blockingAreaForMainBuilding = {
    val ul = area.upperLeft.movedBy(-3, -3)
    val lr = area.lowerRight.movedBy(3, 3)
    Area(ul, lr)
  }

  private val remainingResources = oncePerTick {
    nativeUnit.getResources
  }

  def nonEmpty = remaining > 8

  def remaining = if (isInGame) remainingResources.get else 0
}
