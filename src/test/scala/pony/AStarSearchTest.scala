package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.astar.{AStarSearch, GridNode2DInt, Heuristics}

import scala.collection.immutable.BitSet

class AStarSearchTest extends Specification with MustMatchers {

  def is =
    s2"""
       |A straight corridor is solved through neighbouring tiles $corridor
       |The full solution starts at the start and excludes the target $solutionEnds
       |A wall is passed through its only gap $wallGap
       |A walled-off target is unsolvable and reports a reachable node $unsolvable
       |A repeated search on the same graph returns the same path $repeatable
       |Shortcuts drop every node a direct line can bypass $shortcuts
       |The zero heuristic still finds the target $zeroHeuristic
       |PathFinder routes around a wall on a Grid2D $pathFinderAroundWall
       """.stripMargin

  /** A 4-connected grid; `blocked` tiles get no node. */
  private final class Board(
      cols: Int,
      rows: Int,
      blocked: Set[(Int, Int)] = Set.empty,
      shortcuts: Boolean = false,
      heuristics: Option[Heuristics[GridNode2DInt]] = None
  ) {
    val nodes: Map[(Int, Int), GridNode2DInt] = (for {
      x <- 0 until cols
      y <- 0 until rows
      if !blocked((x, y))
    } yield (x, y) -> node(x, y)).toMap

    nodes.foreach { case ((x, y), here) =>
      Seq((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)).flatMap(nodes.get).foreach(here.addNeighbour)
    }

    def search(from: (Int, Int), to: (Int, Int)) =
      new AStarSearch[GridNode2DInt](nodes(from), nodes(to)).performSearch()

    private def node(x: Int, y: Int): GridNode2DInt = new GridNode2DInt(x, y) {
      override def suggestHeuristics: Heuristics[GridNode2DInt] = heuristics.getOrElse(manhattan)

      override def supportsShortcuts: Boolean = shortcuts

      override def canReachDirectly(other: GridNode2DInt): Boolean = true
    }
  }

  private val manhattan: Heuristics[GridNode2DInt] =
    (from, target) => math.abs(from.x - target.x) + math.abs(from.y - target.y)

  private def coords(path: Seq[GridNode2DInt]) = path.map(n => (n.x, n.y))

  private def adjacent(path: Seq[(Int, Int)]) = path.sliding(2).forall {
    case Seq((ax, ay), (bx, by)) => math.abs(ax - bx) + math.abs(ay - by) == 1
    case _                       => true
  }

  def corridor = {
    val search = new Board(6, 1).search((0, 0), (5, 0))
    (search.isSolved must beTrue) and (coords(search.fullSolution) === (0 to 4).map(_ -> 0))
  }

  def solutionEnds = {
    val search = new Board(5, 5).search((0, 0), (4, 4))
    val path   = coords(search.fullSolution)
    (path.head === (0, 0)) and (path.contains((4, 4)) must beFalse) and (adjacent(path :+ (4, 4)) must beTrue) and
      ((search.targetOrNearestReachable.x, search.targetOrNearestReachable.y) === (4, 4))
  }

  def wallGap = {
    val wall   = (0 until 5).filterNot(_ == 3).map(2 -> _).toSet
    val search = new Board(5, 5, wall).search((0, 0), (4, 0))
    val path   = coords(search.fullSolution) :+ (4, 0)
    (search.isSolved must beTrue) and (path.contains((2, 3)) must beTrue) and (adjacent(path) must beTrue)
  }

  def unsolvable = {
    val wall    = (0 until 5).map(2 -> _).toSet
    val board   = new Board(5, 5, wall)
    val search  = board.search((0, 0), (4, 4))
    val nearest = (search.targetOrNearestReachable.x, search.targetOrNearestReachable.y)
    (search.isUnsolvable must beTrue) and (search.isSolved must beFalse) and (nearest._1 must be_<(2))
  }

  def repeatable = {
    val board  = new Board(6, 6, Set((2, 2), (3, 2), (2, 3)))
    val first  = coords(board.search((0, 0), (5, 5)).fullSolution)
    val second = coords(board.search((0, 0), (5, 5)).fullSolution)
    (first === second) and (first must not(beEmpty))
  }

  def shortcuts = {
    val search = new Board(6, 1, shortcuts = true).search((0, 0), (5, 0))
    (coords(search.fullSolution).size === 5) and (coords(search.solution) === Seq(0 -> 0, 4 -> 0))
  }

  def zeroHeuristic = {
    val search = new Board(4, 4, heuristics = Some(Heuristics.none[GridNode2DInt])).search((0, 0), (3, 3))
    (search.isSolved must beTrue) and (adjacent(coords(search.fullSolution) :+ (3, 3)) must beTrue)
  }

  def pathFinderAroundWall = {
    val cols = 12
    val rows = 12
    val wall = (0 until 10).map(y => 6 + y * cols)
    val grid = new Grid2D(cols, rows, BitSet(wall*))
    val path = PathFinder.on(grid, isOnGround = true)
      .findSimplePathNow(MapTilePosition(1, 1), MapTilePosition(10, 1))
    (path.map(_.solved) === Some(true)) and
      (path.toSeq.flatMap(_.waypoints).forall(grid.free) must beTrue) and
      (path.map(_.waypoints.head) === Some(MapTilePosition(1, 1))) and
      (path.map(_.bestEffort) === Some(MapTilePosition(10, 1)))
  }
}
