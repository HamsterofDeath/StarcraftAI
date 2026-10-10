package pony
package geometry

import bwapi.Position

case class MapPosition(x: Int, y: Int) extends HasXY {
  def toNative = new Position(x, y)
}
