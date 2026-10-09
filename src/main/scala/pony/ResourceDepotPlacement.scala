package pony

private[pony] object ResourceDepotPlacement {
  def permitted(area: Area, resourceDepot: Boolean, resourceBuffer: Grid2D): Boolean =
    !resourceDepot || resourceBuffer.free(area)
}
