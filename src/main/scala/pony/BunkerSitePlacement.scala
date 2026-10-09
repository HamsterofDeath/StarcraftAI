package pony

private[pony] object BunkerSitePlacement {
  // This grid contains terrain, resources, buildings, mining paths and reservations, not mobile SCVs.
  def permitted(area: Area, staticGrid: Grid2D): Boolean = {
    if (!staticGrid.inBounds(area) || !staticGrid.free(area)) false
    else if (area.growBy(1).outline.forall(staticGrid.freeAndInBounds)) true
    else {
      val blocked = staticGrid.mutableCopy
      blocked.block_!(area)
      blocked.areaCountExpensive == staticGrid.areaCount
    }
  }
  def permittedTogether(areas: Seq[Area], staticGrid: Grid2D): Boolean = {
    val blocked = staticGrid.mutableCopy
    areas.forall { area =>
      val safe = permitted(area, blocked)
      if (safe) blocked.block_!(area)
      safe
    }
  }
}
