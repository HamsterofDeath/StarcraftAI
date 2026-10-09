package pony

case class Area(upperLeft: MapTilePosition, sizeOfArea: Size) {
  def height = sizeOfArea.y

  def width = sizeOfArea.x

  val lowerRight = upperLeft.movedBy(sizeOfArea).movedBy(-1, -1)
  val edges      = upperLeft ::
                   MapTilePosition(lowerRight.x, upperLeft.y) ::
                   lowerRight ::
                   MapTilePosition(upperLeft.x, lowerRight.y) ::
                   Nil
  val center     = MapPosition((upperLeft.mapX + lowerRight.mapX) / 2,
    (upperLeft.mapY + lowerRight.mapY) / 2)
  val centerTile = MapTilePosition((upperLeft.x + lowerRight.x) / 2, (upperLeft.y + lowerRight.y)
                                                                     / 2)

  def moveTo(e: MapTilePosition) = copy(upperLeft = e)

  def extendedBy(tiles: Int) = {
    growBy(tiles)
  }

  def growBy(tiles: Int) = {
    tiles match {
      case 0 => this
      case n => Area(upperLeft.movedBy(-n, -n), lowerRight.movedBy(n, n))
    }
  }

  def distanceTo(tilePosition: MapTilePosition) = {
    // TODO optimize
    outline.minBy(_.distanceSquaredTo(tilePosition)).distanceTo(tilePosition)
  }

  def distanceTo(area: Area) = {
    closestDirectConnection(area).length
  }

  def closestDirectConnection(area: Area): Line = {
    // TODO optimize
    val from = outline.minBy { p =>
      area.outline.minBy(_.distanceSquaredTo(p)).distanceTo(p)
    }

    val to = area.outline.minBy { p =>
      outline.minBy(_.distanceSquaredTo(p)).distanceTo(p)
    }
    Line(from, to)

  }

  def outline: Iterable[MapTilePosition] = {
    new Iterable[MapTilePosition] {
      override def iterator: Iterator[MapTilePosition] = {
        val topAndBottom = (0 until sizeOfArea.x).iterator.flatMap { x =>
          Iterator(MapTilePosition.shared(upperLeft.x + x, upperLeft.y),
            MapTilePosition.shared(upperLeft.x + x, upperLeft.y + sizeOfArea.y))
        }
        val sides = (1 until sizeOfArea.y - 1).iterator.flatMap { y =>
          Iterator(MapTilePosition.shared(upperLeft.x, upperLeft.y + y),
            MapTilePosition.shared(upperLeft.x + sizeOfArea.x, upperLeft.y + y))
        }
        topAndBottom ++ sides
      }

      override def isEmpty = false
    }

  }

  def anyTile = upperLeft

  def closestDirectConnection(elem: StaticallyPositioned): Line =
    closestDirectConnection(elem.area)

  def tiles: Iterable[MapTilePosition] = new Iterable[MapTilePosition] {
    override def iterator: Iterator[MapTilePosition] =
      sizeOfArea.points.iterator.map(_.movedBy(upperLeft))
  }

  def describe = s"$upperLeft/$lowerRight"
}

object Area {
  def apply(upperLeft: MapTilePosition, lowerRight: MapTilePosition): Area = {
    Area(upperLeft, Size.shared(lowerRight.x - upperLeft.x + 1, lowerRight.y - upperLeft.y + 1))
  }
}
