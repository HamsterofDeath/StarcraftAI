package pony
package pathing

import pony.geometry.{Grid2D, MapTilePosition}

import scala.collection.mutable

case class Path(
    waypoints: Seq[MapTilePosition],
    solved: Boolean,
    solvable: Boolean,
    bestEffort: MapTilePosition,
    requestedTarget: MapTilePosition,
    unsafeTarget: MapTilePosition
)(basedOn: Grid2D) {
  lazy val length = {
    lengthOf(waypoints.iterator)
  }
  private val cache = mutable.HashMap.empty[MapTilePosition, Option[Double]]

  def distanceToFinalTargetViaPath(from: MapTilePosition) = {
    cache.getOrElseUpdate(
      from, {
        closestWaypoint(from).map { first =>
          lengthOf(waypoints.iterator.dropWhile(_ != first)) + first.distanceTo(from)
        }
      }
    )
  }

  def closestWaypoint(of: MapTilePosition) = {
    // not perfect, but good enough
    waypoints.iterator
      .filter(e => basedOn.connectedByLine(e, of))
      .minByOpt(_.distanceSquaredTo(of))
  }

  private def lengthOf(path: Iterator[MapTilePosition]) = {
    path.sliding(2, 1).map {
      case Seq(a, b)          => a.distanceTo(b)
      case Seq(singleElement) => 0.0
    }.sum
  }

  def head = waypoints.head

  def isPerfectSolution = solved
}
