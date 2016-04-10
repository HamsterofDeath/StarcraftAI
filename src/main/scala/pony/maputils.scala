package pony

import bwapi.TilePosition

import scala.collection.mutable

class Grid2D(val cols: Int, val rows: Int, areaDataBitSet: scala.collection.BitSet,
             protected val containsBlocked: Boolean = true) extends Serializable {
  self =>
  def asMutable = mutableCopy

  def or(other: Grid2D) = mutableCopy.or_!(other.asMutable).guaranteeImmutability

  private lazy val lazyAreas = new AreaHelper(self).findFreeAreas

  def anyFreeInSpiral(tile: MapTilePosition, size: Int) = {
    spiralAround(tile, 3).exists(free)
  }

  def invertedMutable = { reverseView.mutableCopy }

  def reverseView: Grid2D = new Grid2D(cols, rows, areaDataBitSet, false)

  def mutableCopy = new
      MutableGrid2D(cols, rows, mutable.BitSet.fromBitMaskNoCopy(areaDataBitSet.toBitMask),
        containsBlocked)

  def areasIntersecting(area: Area): Set[Grid2D] = {
    import scala.collection.breakOut
    area.tiles.flatMap(areaOf)(breakOut)
  }

  def areaOf(tile: MapTilePosition) = areas.find(e => e.inBounds(tile) && e.free(tile))

  def getAreaOf(tile: MapTilePosition) = areaOf(tile).getOr(s"$tile not free in $self")

  def areas = lazyAreas

  def inBounds(p: MapTilePosition): Boolean = inBounds(p.x, p.y)

  def free(p: MapTilePosition): Boolean = free(p.x, p.y)

  def cuttingAreas(area: Area) = {
    var found = Option.empty[Grid2D]
    area.outline.exists { where =>
      val check = areaOf(where)
      if (found.isEmpty && check.isDefined) {
        found = check
      }
      check.isDefined && check != found
    }
  }

  def nearestFreeBlock(center: MapTilePosition, radius: Int) = {
    spiralAround(center).find { center =>
      containsAndFree(Area(center.movedBy(-radius, -radius), center.movedBy(radius, radius)))
    }
  }

  def containsAndFree(a: Area): Boolean = a.tiles.forall(containsAndFree)

  def containsAndFree(p: MapTilePosition): Boolean = inBounds(p) && free(p)

  def insideBounds(t: MapTilePosition) = {
    t.y >= 0 && t.x < cols && t.y >= 0 && t.y < rows
  }

  def areInSameWalkableArea(a: MapTilePosition, b: MapTilePosition) =
    areaOf(a).exists(_.free(b))

  def emptySameSize(blocked: Boolean) = new MutableGrid2D(cols, rows, mutable.BitSet.empty, blocked)

  def nearestFree(p: MapTilePosition) = {
    spiralAround(p, 80).find(free)
  }

  def spiralAround(center: MapTilePosition, size: Int = 45) = {
    geoHelper.iterateBlockSpiralClockWise(center, size)
  }

  def geoHelper = new GeometryHelpers(cols, rows)

  def nearestFreeNoGap(p: MapTilePosition) = {
    spiralAround(p, 80).filter { candidate =>
      val stats = countBlockedOnLine(Line(p, candidate))
      stats.free > stats.blocked
    }.find(free)
  }

  def blockedMutableCopy = new MutableGrid2D(cols, rows, mutable.BitSet.empty, false)

  def guaranteeImmutability = this

  override def toString = s"$cols*$rows, $freeCount free"

  def freeCount = if (containsBlocked) size - areaDataBitSet.size else areaDataBitSet.size

  def size = cols * rows

  def outlineFree(a: Area): Boolean = a.outline.forall(free)

  def outlineFreeAndInBounds(a: Area): Boolean = inBounds(a) && a.outline.forall(free)

  def inBounds(area: Area): Boolean = area.edges.forall(inBounds)

  def freeAndInBounds(a: Area): Boolean = a.tiles.forall(e => inBounds(e) && free(e))

  def anyBlocked(a: Area): Boolean = !free(cut(a))

  def cut(a: Area) = {
    val ul = MapTilePosition(a.upperLeft.x max 0, a.upperLeft.y max 0)
    val lr = MapTilePosition(a.lowerRight.x min (cols - 1), a.lowerRight.y min (rows - 1))
    Area(ul, lr)
  }

  def anyBlockedOrOutside(a: Area): Boolean = !inBounds(a) || !free(a)

  def free(p: TilePosition): Boolean = free(p.getX, p.getY)

  def anyBlockedOnLine(center: MapTilePosition, from: HasXY, to: HasXY): Boolean = {
    val absoluteFrom = center.movedBy(from)
    val absoluteTo = center.movedBy(to)
    anyBlockedOnLine(Line(absoluteFrom, absoluteTo))
  }

  def anyBlockedOnLine(line: Line): Boolean = {
    AreaHelper.traverseTilesOfLine(line.a, line.b, (x, y) => {
      if (inBounds(x, y) && blocked(x, y)) Some(true) else None
    }, false)
  }

  def blocked(x: Int, y: Int): Boolean = !free(x, y)

  def free(x: Int, y: Int): Boolean = {
    assert(inBounds(x, y), s"$x / $y is not inside $cols, $rows")
    val coord = x + y * cols
    if (containsBlocked) !areaDataBitSet(coord) else areaDataBitSet(coord)
  }

  def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < cols && y >= 0 && y < rows

  def countBlockedOnLine(a: MapTilePosition, b: MapTilePosition): LineInfo = {
    val line = Line(a, b)
    countBlockedOnLine(line)
  }

  def countBlockedOnLine(line: Line): LineInfo = {
    var blockedCount = 0
    var freeCount = 0
    AreaHelper.traverseTilesOfLine(line.a, line.b, (x, y) => {
      if (inBounds(x, y)) {
        if (blocked(x, y)) {
          blockedCount += 1
        } else {
          freeCount += 1
        }
      }
    })
    LineInfo(line, blockedCount, freeCount)
  }

  def connectedByLine(a: MapTilePosition, b: MapTilePosition) = {
    AreaHelper.directLineOfSight(a, b, this)
  }

  def blockedCount = size - freeCount

  def mkString: String = mkString('x')

  def mkString(blockedDisplay: Char) = {
    0 until rows map { y =>
      0 until cols map { x =>
        if (free(x, y)) " " else blockedDisplay
      } mkString
    } mkString "\n"

  }

  def ensureContainsBlocked = if (containsBlocked)
    this
  else {
    val allBlockedIndexes = new mutable.BitSet ++= allBlocked.map(tileToIndex)
    new Grid2D(cols, rows, allBlockedIndexes.toImmutable)
  }

  def allBlocked = if (containsBlocked) bitSetToTiles
  else allIndexes.filterNot(areaDataBitSet).map(indexToTile)

  private def allIndexes = Iterator.range(0, size)

  private def indexToTile(index: Int) = MapTilePosition.shared(index % cols, index / cols)

  private def bitSetToTiles = areaDataBitSet.iterator.map { index =>
    MapTilePosition.shared(index % cols, index / cols)
  }

  private def tileToIndex(mp: MapTilePosition) = mp.x + mp.y * cols

  def blocked(index: Int) = !free(index)

  def free(index: Int) = if (containsBlocked) !areaDataBitSet(index) else areaDataBitSet(index)

  def minAreaSize(i: Int) = {
    val mut = mutableCopy
    val tooSmall = areas.filter(_.freeCount < i)
    tooSmall.foreach { area =>
      area.allFree.foreach(mut.block_!)
    }
    mut.asReadOnlyView
  }

  def allFree = if (containsBlocked) allIndexes.filterNot(areaDataBitSet).map(indexToTile)
  else bitSetToTiles

  def free(position: MapTilePosition, area: Size): Boolean = {
    area.points.forall { p =>
      free(p.movedBy(position))
    }
  }

  def freeAndInBounds(position: MapTilePosition, size: Size): Boolean = {
    val area = Area(position, size)
    inBounds(area) && free(area)
  }

  def free(a: Area): Boolean = a.tiles.forall(free)

  def freeAndInBounds(position: MapTilePosition): Boolean = {
    inBounds(position) && free(position)
  }

  def zoomedOut = {
    val bits = mutable.BitSet.empty
    val subCols = cols / 4
    val subRows = rows / 4
    def squareFree(x: Int, y: Int) = free(x * 4, y * 4) && free(x * 4 + 1, y * 4) &&
                                     free(x * 4, y * 4 + 1) &&
                                     free(x * 4 + 1, y * 4 + 1)
    for (x <- 0 until subCols; y <- 0 until subRows
         if !squareFree(x, y)) {
      bits += (x + y * subCols)
    }
    new Grid2D(subCols, subRows, bits)
  }

  def blocked = size - walkable

  def walkable = areaDataBitSet.size

  def includesAndBlocked(p: MapTilePosition): Boolean = inBounds(p) && blocked(p)

  def blocked(p: MapTilePosition): Boolean = !free(p)

  def containsAsData(x: Int, y: Int): Boolean = !free(x, y)

  def all: Iterator[MapTilePosition] = new Iterator[MapTilePosition] {
    private var index = 0
    private val max   = self.size

    override def hasNext = index < max

    override def next() = {
      val ret = MapTilePosition.shared(index % cols, index / cols)
      index += 1
      ret
    }
  }

  def areaCount = areas.size

  case class LineInfo(line: Line, blocked: Int, free: Int) {
    def freePercentage = free.toDouble / (blocked + free)
  }

}

class MutableGrid2D(cols: Int, rows: Int, bitSet: mutable.BitSet,
                    bitSetContainsBlocked: Boolean = true)
  extends Grid2D(cols, rows, bitSet, bitSetContainsBlocked) {
  def addOutlineToBlockedTiles_!() = {
    allBlocked.flatMap(_.asArea.growBy(1).tiles).toSet.foreach((e: MapTilePosition) => block_!(e))
    this
  }

  override def asMutable = this

  def areaSize(anyContained: MapTilePosition) = {
    val isFree = free(anyContained)
    val on = if (isFree) this else reverseView
    AreaHelper.freeAreaSize(anyContained, on)
  }

  def anyFree = allFree.toStream.headOption

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
    val absoluteTo = center.movedBy(to)
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
