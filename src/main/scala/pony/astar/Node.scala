package pony.astar

import scala.compiletime.uninitialized

object Node {

  /** The ordinal is stored in the two low bits of a node's packed cost, so the order matters. */
  enum State {
    case Unknown, Closed, Open
  }

  private val States     = State.values
  private val StateMask  = 3
  private val ClearState = ~3
}

/** A search graph node that packs its path cost (30 bits) and search state (2 bits) into one Int. */
@SerialVersionUID(0L)
abstract class Node[T <: Node[T]] extends Serializable {
  this: T =>

  import Node.*

  private var parentNode: T = uninitialized
  private var costAndState  = 0

  /** @param parent a directly connected node; the result is undefined for any other node. */
  def evalCostFromParent(parent: T): Int

  def neighbours: collection.Seq[T]

  def suggestHeuristics: Heuristics[T]

  def supportsShortcuts: Boolean

  def canReachDirectly(node: T): Boolean

  def addNeighbour(node: T): Unit

  def removeNeighbour(node: T): Unit

  /** Disconnects this node from all of its neighbours. */
  def detach(): Unit

  def estimatedTotalCost(target: T, heuristics: Heuristics[T]): Int =
    wholePathCost + heuristics.estimateCost(this, target)

  def setNewParent(parent: T): Unit = {
    parentNode = parent
    setPathCost(parent.wholePathCost + evalCostFromParent(parent))
  }

  def initAsStart(): Unit = {
    parentNode = uninitializedParent
    setState(State.Open)
  }

  def initForSearch(): Unit = {
    setState(State.Unknown)
    parentNode = uninitializedParent
    setPathCost(0)
  }

  def state: State = States(costAndState & StateMask)

  def isClosed: Boolean = state == State.Closed

  def isOpen: Boolean = state == State.Open

  def open(): Unit = setState(State.Open)

  def close(): Unit = setState(State.Closed)

  def hasParent: Boolean = parentNode != null

  def wholePathCost: Int = costAndState >>> 2

  /** The parent chain from the direct parent up to and including the search start; never this node. */
  def ancestors: Iterator[T] = Iterator.iterate(parentNode)(_.parentNode).takeWhile(_ != null)

  private def uninitializedParent: T = null.asInstanceOf[T]

  private def setState(newState: State): Unit = {
    costAndState = (costAndState & ClearState) | newState.ordinal
  }

  private def setPathCost(cost: Int): Unit = {
    costAndState = (costAndState & StateMask) | (cost << 2)
  }
}
