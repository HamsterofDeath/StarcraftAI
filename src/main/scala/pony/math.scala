package pony

trait HasXY {
  def x: Int
  def y: Int
}
case class Point(x: Int, y: Int) extends HasXY {
  def movedBy(other: HasXY) = Point.shared(x + other.x, y + other.y)
  def distanceTo(other: Point) = {
    val xDiff = x - other.x
    val yDiff = y - other.y
    math.sqrt(xDiff * xDiff + yDiff * yDiff)
  }
  override def toString = s"($x,$y)"
}
object Point {
  val points = Array.tabulate(256 * 4, 256 * 4)((x, y) => Point(x, y))
  def shared(x: Int, y: Int) = points(x)(y)
}

case class Size(x: Int, y: Int) extends HasXY
object Size {
  val sizes = Array.tabulate(5, 5)((x, y) => Size(x, y))
  def shared(x: Int, y: Int) = sizes(x)(y)
}

case class Area(upperLeft: Point, sizeOfArea: Size) {
  def outline:Traversable[Point] = {
    new Traversable[Point] {
      override def foreach[U](f: (Point) => U): Unit = {
        (0 until sizeOfArea.x).foreach { x =>
          f(Point.shared(upperLeft.x + x, upperLeft.y))
          f(Point.shared(upperLeft.x + x, upperLeft.y + sizeOfArea.y))
        }
        (1 until sizeOfArea.y-1).foreach { y =>
          f(Point.shared(upperLeft.x, upperLeft.y + y))
          f(Point.shared(upperLeft.x + sizeOfArea.x, upperLeft.y + y))
        }
      }
    }

  }

  val lowerRight = upperLeft.movedBy(sizeOfArea)
  def describe = s"$upperLeft/$lowerRight"
}