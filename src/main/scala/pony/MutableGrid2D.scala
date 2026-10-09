package pony

import scala.collection.mutable

class MutableGrid2D(
    cols: Int,
    rows: Int,
    bitSet: mutable.BitSet,
    bitSetContainsBlocked: Boolean = true
) extends Grid2D(cols, rows, bitSet, bitSetContainsBlocked) {
  def addOutlineToBlockedTiles_!() = {
    allBlocked.flatMap(_.asArea.growBy(1).tiles).toSet.foreach((e: MapTilePosition) => block_!(e))
    this
  }

  override def asMutable = this

  def areaSize(anyContained: MapTilePosition) = {
    val isFree = free(anyContained)
    val on     = if (isFree) this else reverseView
    AreaHelper.freeAreaSize(anyContained, on)
  }

  def anyFree = allFree.iterator.nextOption()

  override def areas = {
    error(s"Check this!!!", doIt = true)
    areasExpensive
  }

  override def areaCount = {
    error(s"Check this!!!", doIt = true)
    areaCountExpensive
  }

  def areaCountExpensive = {
    areasExpensive.size
  }

  def areasExpensive = {
    new AreaHelper(this).findFreeAreas
  }

  def block_!(a: MapTilePosition, b: MapTilePosition): Unit = {
    AreaHelper.traverseTilesOfLine(a, b, block_!)
  }

  def block_!(center: MapTilePosition, grow: Int): Unit = {
    block_!(Area(center, Size(1, 1).growBy(grow)))
  }

  def block_!(center: MapTilePosition, from: HasXY, to: HasXY): Unit = {
    val absoluteFrom = center.movedBy(from)
    val absoluteTo   = center.movedBy(to)
    block_!(Line(absoluteFrom, absoluteTo))
  }

  def block_!(line: Line): Unit = {
    AreaHelper.traverseTilesOfLine(line.a, line.b, block_!)
  }

  def asReadOnlyView: Grid2D = this

  override def guaranteeImmutability = new Grid2D(cols, rows, bitSet, containsBlocked)

  def or_!(other: MutableGrid2D) = {
    if (containsBlocked == other.containsBlocked) {
      bitSet |= other.data
    } else {
      bitSet |= mutable.BitSet.fromBitMask(other.data.toBitMask.map(~_))
    }
    this
  }

  protected def data = bitSet

  def block_!(area: Area): Unit = {
    area.tiles.foreach { p => block_!(p.x, p.y) }
  }

  def free_!(area: Area): Unit = {
    area.tiles.foreach { p => free_!(p.x, p.y) }
  }

  def free_!(x: Int, y: Int): Unit = {
    if (inArea(x, y)) {
      val where = xyToIndex(x, y)
      if (containsBlocked) {
        bitSet -= where
      } else {
        bitSet += where
      }
    }
  }

  def block_!(tile: MapTilePosition): Unit = {
    block_!(tile.x, tile.y)
  }

  def block_!(x: Int, y: Int): Unit = {
    if (inArea(x, y)) {
      val where = xyToIndex(x, y)
      if (containsBlocked) {
        bitSet += where
      } else {
        bitSet -= where
      }
    }
  }

  private def xyToIndex(x: Int, y: Int) = x + y * cols

  def inArea(x: Int, y: Int) = x >= 0 && y >= 0 && x < cols && y < rows

  def free_!(tile: MapTilePosition): Unit = {
    free_!(tile.x, tile.y)
  }
}
