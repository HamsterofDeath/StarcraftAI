package pony

trait HasXY {
  def x: Int
  def y: Int

  def distanceToIsLess(other: HasXY, dst: Int) = distanceSquaredTo(other) < dst * dst

  def distanceToIsMore(other: HasXY, dst: Int) = distanceSquaredTo(other) > dst * dst

  def distanceSquaredTo(other: HasXY) = {
    val xDiff = x - other.x
    val yDiff = y - other.y
    xDiff * xDiff + yDiff * yDiff
  }

  def distanceTo(other: HasXY) = math.sqrt(distanceSquaredTo(other))

}
