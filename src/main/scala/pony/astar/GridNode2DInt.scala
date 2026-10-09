package pony.astar

import scala.collection.mutable

/** A node on an integer tile grid; subclasses decide heuristics, shortcuts and direct reachability. */
@SerialVersionUID(0L)
abstract class GridNode2DInt(val x: Int, val y: Int) extends Node[GridNode2DInt] {
  private val connected = new mutable.ArrayBuffer[GridNode2DInt](8)

  override def evalCostFromParent(parent: GridNode2DInt): Int =
    math.sqrt((parent.x * x + parent.y * y).toDouble).toInt

  override def neighbours: collection.Seq[GridNode2DInt] = connected

  override def addNeighbour(node: GridNode2DInt): Unit = connected += node

  override def removeNeighbour(node: GridNode2DInt): Unit = connected -= node

  override def detach(): Unit = connected.foreach(_.removeNeighbour(this))
}
