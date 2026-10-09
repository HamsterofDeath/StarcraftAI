package pony

import pony.astar.{AStarSearch, GridNode2DInt, Heuristics}

import scala.language.implicitConversions

object PathFinder {

  implicit def convBack(gn: GridNode2DInt): MapTilePosition = MapTilePosition
                                                              .shared(gn.x, gn.y)

  def on(map: Grid2D, isOnGround: Boolean) = {
    new PathFinder(map, isOnGround)
  }

  def to2DArray(grid: Grid2D) = {

    val asGrid = Array.ofDim[GridNode2DInt](grid.cols, grid.rows)
    implicit def conv(mtp: MapTilePosition): GridNode2DInt = {
      asGrid(mtp.x)(mtp.y)
    }
    // fill the grid
    grid.allFree.foreach { e =>
      asGrid(e.x)(e.y) = new GridNode2DInt(e.x, e.y) {
        override def suggestHeuristics: Heuristics[GridNode2DInt] = {
          (from: GridNode2DInt, to: GridNode2DInt) => from.distanceTo(to).toInt
        }

        override def supportsShortcuts: Boolean = true

        override def canReachDirectly(node: GridNode2DInt): Boolean = {
          grid.connectedByLine(e, node)
        }
      }
    }
    // connect the grid
    grid.allFree.foreach { mtp =>
      val (left, right, up, down) = mtp.leftRightUpDown
      val leftFree = grid.containsAndFree(left)
      val rightFree = grid.containsAndFree(right)
      val upFree = grid.containsAndFree(up)
      val downFree = grid.containsAndFree(down)
      val here = asGrid(mtp.x)(mtp.y)
      if (leftFree) here.addNeighbour(left)
      if (rightFree) here.addNeighbour(right)
      if (upFree) here.addNeighbour(up)
      if (downFree) here.addNeighbour(down)
      if (leftFree && upFree) {
        val moved = mtp.movedBy(-1, -1)
        if (grid.free(moved)) {
          here.addNeighbour(moved)
        }
      }
      if (leftFree && downFree) {
        val moved = mtp.movedBy(-1, 1)
        if (grid.free(moved)) {
          here.addNeighbour(moved)
        }
      }
      if (rightFree && upFree) {
        val moved = mtp.movedBy(1, -1)
        if (grid.free(moved)) {
          here.addNeighbour(moved)
        }
      }
      if (rightFree && downFree) {
        val moved = mtp.movedBy(1, 1)
        if (grid.free(moved)) {
          here.addNeighbour(moved)
        }
      }
    }

    asGrid
  }

}

class PathFinder(on: Grid2D, isOnGround: Boolean) {

  import PathFinder._

  def this(mapLayers: MapLayers, safe: Boolean, ground: Boolean) = {
    this({
      (safe, ground) match {
        case (true, true) =>
          mapLayers.safeGround.guaranteeImmutability
        case (false, true) =>
          mapLayers.freeWalkableIgnoringMobiles.guaranteeImmutability
        case (false, false) =>
          mapLayers.emptyGrid.guaranteeImmutability
        case (true, false) =>
          mapLayers.safeAir.guaranteeImmutability
      }
    }, ground)
  }

  def findPath(from: MapTilePosition, to: MapTilePosition) = {
    findPaths(from, to, 1)
  }

  def findSimplePathNow(from: MapTilePosition, to: MapTilePosition,
                        tryFixPath: Boolean = true) = {
    findPathNow(from, to, 1, tryFixPath).flatMap(_.paths.headOption)
  }

  def findPathNow(from: MapTilePosition, to: MapTilePosition,
                  paths: Int = 1, tryFixPath: Boolean = true): Option[Paths] = {
    evalPath(from, to, paths, tryFixPath)
  }

  /** Route to the goal across walkable regions without clamping it into the start's area. */
  def findUnclampedPathNow(from: MapTilePosition, to: MapTilePosition): Option[Paths] = {
    val fromFixed = on.nearestFree(from)
    val toFixed = if (on.containsAndFree(to)) Some(to) else on.nearestFree(to)
    for (a <- fromFixed; b <- toFixed) yield spawn.findPaths(a, b, 1, to)
  }

  def findPaths(from: MapTilePosition, to: MapTilePosition, paths: Int = 10,
                tryFixPath: Boolean = true) = BWFuture {
    findPathNow(from, to, paths, tryFixPath)
  }

  private def evalPath(from: MapTilePosition, to: MapTilePosition, paths: Int,
                       tryFixPath: Boolean): Option[Paths] = {
    val fromFixed = {if (tryFixPath) on.nearestFree(from) else Some(from)}
    val toFixed = {
      if (tryFixPath) {
        fromFixed.flatMap { e =>
          on.areaOf(e).flatMap(_.nearestFreeNoGap(to))
        }.orElse(on.nearestFree(to))
      } else {Some(to)}
    }
    warn(s"Could not fix start $from", fromFixed.isEmpty)
    warn(s"Could not fix goal $to", toFixed.isEmpty)
    val unsafeTarget = to
    for (a <- fromFixed; b <- toFixed) yield spawn.findPaths(a, b, paths, unsafeTarget)
  }

  def spawn = new AstarPathFinder(to2DArray(on), isOnGround)

  class AstarPathFinder(grid: Array[Array[GridNode2DInt]], isOnGround: Boolean) {

    def basedOn = on

    def findPaths(from: MapTilePosition, to: MapTilePosition, width: Int,
                  unsafeTarget: MapTilePosition) = {
      trace(s"Searching path from $from to $to", tick > 10)
      val finder = new AStarSearch[GridNode2DInt](from, to)
      var first = Option.empty[Path]
      def pathFrom(seq: Seq[MapTilePosition]) = {
        Path(seq, finder.isSolved, !finder.isUnsolvable, finder.targetOrNearestReachable, to,
          unsafeTarget)(on)
      }
      val paths = (0 until width).iterator.map { _ =>
        finder.performSearch()
        val waypoints = finder.fullSolution
                        .map(e => e: MapTilePosition)
                        .sliding(1, 4)
                        .flatten
                        .toVector
        //block the path, then search again to get streets
        finder.fullSolution.drop(15).dropRight(10).foreach(_.detach())
        if (first.isEmpty) first = Some(
          pathFrom(waypoints :+ (finder.targetOrNearestReachable: MapTilePosition)))
        waypoints
      }.takeWhile { candidate =>
        candidate.forall { pointOnLine =>
          first.get.waypoints.exists { old =>
            on.connectedByLine(old, pointOnLine)
          }
        }
      }.toVector
      Paths(paths.map(pathFrom), isOnGround)
    }

    private implicit def conv(mtp: MapTilePosition): GridNode2DInt = {
      val ret = grid(mtp.x)(mtp.y)
      assert(ret != null, s"Map tile $mtp is not free!")
      ret
    }
  }

}
