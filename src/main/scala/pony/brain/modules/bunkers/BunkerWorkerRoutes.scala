package pony
package brain
package modules
package bunkers

import pony.geometry.{Grid2D, MapTilePosition}
import pony.pathing.PathFinder

private[pony] object BunkerWorkerRoutes {
  def between(from: MapTilePosition, to: MapTilePosition, grid: Grid2D): Option[Vector[MapTilePosition]] = {
    if (!grid.freeAndInBounds(from) || !grid.freeAndInBounds(to)) None
    else if (grid.connectedByLine(from, to)) Some(Vector(from, to))
    else new PathFinder(grid, true).findSimplePathNow(from, to, tryFixPath = false)
      .filter(_.solved).map(p => (Vector(from) ++ p.waypoints :+ to).distinct)
      .filter(route => route.sliding(2).forall(p => p.size < 2 || grid.connectedByLine(p.head, p.last)))
  }
}
