package pony
package geometry

case class Size(x: Int, y: Int) extends HasXY {
  def growBy(i: Int) = Size.shared(x + i, y + i)

  def points: Iterable[MapTilePosition] = new Iterable[MapTilePosition] {
    override def iterator: Iterator[MapTilePosition] = (0 until x).iterator.flatMap { px =>
      (0 until y).iterator.map { py => MapTilePosition.shared(px, py) }
    }
  }
}

object Size {
  val sizes = if (memoryHog) { Array.tabulate(20, 20)((x, y) => Size(x, y)) }
  else {
    Array.empty[Array[Size]]
  }

  def shared(x: Int, y: Int) = if (memoryHog) { sizes(x)(y) }
  else { Size(x, y) }
}
