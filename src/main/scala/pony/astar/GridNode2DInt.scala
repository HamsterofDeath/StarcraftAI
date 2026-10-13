package pony.astar

import scala.collection.mutable

/** A node on an integer tile grid; subclasses decide heuristics, shortcuts and direct reachability. */
@SerialVersionUID(0L)
abstract class GridNode2DInt(val x: Int, val y: Int) extends Node[GridNode2DInt] {
  private val connected = new mutable.ArrayBuffer[GridNode2DInt](8)

  /**
    * The length of the step from a neighbour, in tenths of a tile. The Java original, kept by the port, charged the
    * square root of the two positions' dot product, about the distance from the map's corner: every path leaned towards
    * the top left, and the heuristic, in whole tiles, hardly steered the search. On ICCup Lost_Temple the route from the
    * west main to the east main ran 182 tiles instead of 146, up to 34 tiles off the shortest, and searches opened two
    * to eight times the tiles.
    */
  override def evalCostFromParent(parent: GridNode2DInt): Int =
    if (parent.x == x || parent.y == y) GridNode2DInt.StraightCost else GridNode2DInt.DiagonalCost

  override def neighbours: collection.Seq[GridNode2DInt] = connected

  override def addNeighbour(node: GridNode2DInt): Unit = connected += node

  override def removeNeighbour(node: GridNode2DInt): Unit = connected -= node

  override def detach(): Unit = connected.foreach(_.removeNeighbour(this))
}

object GridNode2DInt {
  val StraightCost = 10
  val DiagonalCost = 14

  /** The cost of the shortest way between two nodes on an open grid with diagonal steps; it never overestimates. */
  def octile(a: GridNode2DInt, b: GridNode2DInt): Int = {
    val dx = math.abs(a.x - b.x)
    val dy = math.abs(a.y - b.y)
    StraightCost * (dx max dy) + (DiagonalCost - StraightCost) * (dx min dy)
  }
}
