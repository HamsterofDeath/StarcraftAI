package pony.astar

/** Estimates the remaining cost between two nodes; A* stays optimal while it never overestimates. */
trait Heuristics[T <: Node[T]] {
  def estimateCost(from: T, target: T): Int
}

object Heuristics {

  /** Estimates every remaining cost as zero, which turns A* into Dijkstra. */
  def none[T <: Node[T]]: Heuristics[T] = (_, _) => 0
}
