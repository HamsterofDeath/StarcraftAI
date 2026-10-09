package pony.astar

import pony.astar.Node.State

import scala.collection.mutable
import scala.compiletime.uninitialized

object AStarSearch {
  private enum Progress {
    case Idle, Running, SolutionFound, NotSolvable
  }
}

/** Reusable A* search between two nodes of one graph; each run resets the nodes it touched. */
final class AStarSearch[T <: Node[T]](start: T, target: T, heuristics: Heuristics[T]) {

  import AStarSearch.Progress

  def this(start: T, target: T) = this(start, target, start.suggestHeuristics)

  private val estimates  = Option(heuristics).getOrElse(Heuristics.none[T])
  private val allOpened  = new mutable.ArrayBuffer[T](1024)
  private val openByCost = new java.util.PriorityQueue[T](
    8192,
    (a: T, b: T) =>
      Integer.compare(a.estimatedTotalCost(target, estimates), b.estimatedTotalCost(target, estimates))
  )

  private var progress         = Progress.Idle
  private var bestFound: T     = uninitialized
  private var shortcutSolution = Vector.empty[T]
  private var completeSolution = Vector.empty[T]

  def performSearch(): this.type = {
    openByCost.clear()
    start.initAsStart()
    open(start)
    progress = Progress.Running
    while (progress == Progress.Running) {
      val best = openByCost.peek()
      bestFound = best
      if (best == null) {
        progress = Progress.NotSolvable
      } else {
        best.close()
        openByCost.remove()
        if (best eq target) {
          progress = Progress.SolutionFound
        } else {
          best.neighbours.foreach { node =>
            node.state match {
              case State.Closed =>
              case State.Open   =>
                if (node.wholePathCost > best.wholePathCost + node.evalCostFromParent(best)) {
                  openByCost.remove(node)
                  node.setNewParent(best)
                  openByCost.add(node)
                }
              case State.Unknown =>
                node.open()
                node.setNewParent(best)
                open(node)
            }
          }
          if (openByCost.isEmpty) progress = Progress.NotSolvable
        }
      }
    }

    val end  = if (target.hasParent) target else bestFound
    val path = end.ancestors.toVector.reverse
    completeSolution = path
    shortcutSolution = withShortcuts(path)

    allOpened.foreach(_.initForSearch())
    allOpened.clear()
    this
  }

  /** The target once it was reached, otherwise the closest node the last search got to. */
  def targetOrNearestReachable: T = progress match {
    case Progress.SolutionFound                    => target
    case Progress.NotSolvable if bestFound != null => bestFound
    case _ => throw new IllegalStateException(s"No completed search ($progress)")
  }

  /** Every node from the start up to the end node, which is excluded. */
  def fullSolution: Vector[T] = completeSolution

  /** The full solution with every node skipped that a direct line can bypass. */
  def solution: Vector[T] = shortcutSolution

  def isSolved: Boolean = progress == Progress.SolutionFound

  def isUnsolvable: Boolean = progress == Progress.NotSolvable

  private def open(node: T): Unit = {
    allOpened += node
    openByCost.add(node)
  }

  private def withShortcuts(path: Vector[T]): Vector[T] = {
    if (path.isEmpty || !path.head.supportsShortcuts) path
    else {
      val remaining = path.toBuffer
      var i         = 0
      while (i < remaining.size - 2) {
        val node = remaining(i)
        while (remaining.size - i > 2 && node.canReachDirectly(remaining(i + 2))) {
          remaining.remove(i + 1)
        }
        i += 1
      }
      remaining.toVector
    }
  }
}
