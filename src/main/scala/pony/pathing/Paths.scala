package pony
package pathing

import pony.geometry.MapTilePosition
import pony.render.Renderer

import pony.brain.Universe

case class Paths(paths: Seq[Path], isGroundPath: Boolean) {
  val pathCount           = paths.size
  val requestedTargetSafe = paths.headOption.map(_.requestedTarget).get
  val unsafeTarget        = paths.headOption.map(_.unsafeTarget).get
  val realisticTarget     = MapTilePosition.average(paths.map(_.bestEffort))
  val anyTarget           = paths.head.bestEffort

  def renderDebug(renderer: Renderer): Unit = {
    paths.foreach { singlePath =>
      var prev = Option.empty[MapTilePosition]
      singlePath.waypoints.zipWithIndex.foreach { case (tile, index) =>
        renderer.drawTextAtTile(s"P$index", tile)
        prev.foreach { p =>
          renderer.drawLineInTile(p, tile)
        }
        prev = Some(tile)
      }
    }
  }

  def toMigration(implicit universe: Universe) = new MigrationPath(this, universe)

  def isEmpty = paths.forall(_.waypoints.isEmpty)

  def minimalDistanceTo(tile: MapTilePosition) = {
    if (paths.isEmpty)
      0.0
    else
      math.sqrt(paths.iterator.flatMap(_.waypoints.map(_.distanceSquaredTo(tile))).min)
  }
}
