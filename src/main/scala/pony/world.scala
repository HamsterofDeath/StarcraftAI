package pony

import bwapi.{Color, Game, Player, Position}
import pony.brain.{ResourceRequestSums, Supplies}

import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class UnitData(in: bwapi.Unit) {
  // todo use this
}

sealed trait SCRace {
  def workerClass: Class[_ <: WorkerUnit]
  def transporterClass: Class[_ <: TransporterUnit]
  def supplyClass: Class[_ <: SupplyProvider]
}

case object Terran extends SCRace {
  override def workerClass: Class[_ <: WorkerUnit] = classOf[SCV]
  override def transporterClass: Class[_ <: TransporterUnit] = classOf[Transporter]
  override def supplyClass: Class[_ <: SupplyProvider] = classOf[SupplyDepot]
}

case object Zerg extends SCRace {
  override def workerClass: Class[_ <: WorkerUnit] = classOf[Drone]
  override def transporterClass: Class[_ <: TransporterUnit] = classOf[Overlord]
  override def supplyClass: Class[_ <: SupplyProvider] = classOf[Overlord]
}

case object Protoss extends SCRace {
  override def workerClass: Class[_ <: WorkerUnit] = classOf[Probe]
  override def transporterClass: Class[_ <: TransporterUnit] = classOf[Shuttle]
  override def supplyClass: Class[_ <: SupplyProvider] = classOf[Pylon]
}

case object Other extends SCRace {
  override def workerClass: Class[_ <: WorkerUnit] = ???
  override def transporterClass: Class[_ <: TransporterUnit] = ???
  override def supplyClass: Class[_ <: SupplyProvider] = ???
}

trait WorldListener {
  def onNukeDetect(unit: Position): Unit = {}

  def onUnitDestroy(unit: bwapi.Unit): Unit = {}

  def onUnitMorph(unit: bwapi.Unit): Unit = {}

  def onUnitRenegade(unit: bwapi.Unit): Unit = {}

  def onPlayerLeft(player: Player): Unit = {}

  def onUnitHide(unit: bwapi.Unit): Unit = {}

  def onPlayerDropped(player: Player): Unit = {}

  def onUnitComplete(unit: bwapi.Unit): Unit = {}

  def onUnitEvade(unit: bwapi.Unit): Unit = {}

  def onUnitDiscover(unit: bwapi.Unit): Unit = {}

  def onUnitShow(unit: bwapi.Unit): Unit = {}

  def onUnitCreate(unit: bwapi.Unit): Unit = {}
}

trait WorldEventDispatcher extends WorldListener {
  private val receivers = ArrayBuffer.empty[WorldListener]

  def listen_!(receiver: WorldListener): Unit = {
    receivers += receiver
  }
  override def onNukeDetect(unit: Position): Unit = {
    super.onNukeDetect(unit)
    receivers.foreach(_.onNukeDetect(unit))
  }
  override def onUnitDestroy(unit: bwapi.Unit): Unit = {
    super.onUnitDestroy(unit)
    receivers.foreach(_.onUnitDestroy(unit))
  }
  override def onUnitMorph(unit: bwapi.Unit): Unit = {
    super.onUnitMorph(unit)
    receivers.foreach(_.onUnitMorph(unit))
  }
  override def onUnitRenegade(unit: bwapi.Unit): Unit = {
    super.onUnitRenegade(unit)
    receivers.foreach(_.onUnitRenegade(unit))
  }
  override def onPlayerLeft(player: Player): Unit = {
    super.onPlayerLeft(player)
    receivers.foreach(_.onPlayerLeft(player))
  }
  override def onUnitHide(unit: bwapi.Unit): Unit = {
    super.onUnitHide(unit)
    receivers.foreach(_.onUnitHide(unit))
  }
  override def onPlayerDropped(player: Player): Unit = {
    super.onPlayerDropped(player)
    receivers.foreach(_.onPlayerDropped(player))
  }
  override def onUnitComplete(unit: bwapi.Unit): Unit = {
    super.onUnitComplete(unit)
    receivers.foreach(_.onUnitComplete(unit))
  }
  override def onUnitEvade(unit: bwapi.Unit): Unit = {
    super.onUnitEvade(unit)
    receivers.foreach(_.onUnitEvade(unit))
  }
  override def onUnitDiscover(unit: bwapi.Unit): Unit = {
    super.onUnitDiscover(unit)
    receivers.foreach(_.onUnitDiscover(unit))
  }
  override def onUnitShow(unit: bwapi.Unit): Unit = {
    super.onUnitShow(unit)
    receivers.foreach(_.onUnitShow(unit))
  }
  override def onUnitCreate(unit: bwapi.Unit): Unit = {
    super.onUnitCreate(unit)
    receivers.foreach(_.onUnitCreate(unit))
  }
}

case class Resources(minerals: Int, gas: Int, supply: Supplies) {
  def supplyTotal = supply.total
  def >(sum: ResourceRequestSums) = (minerals >= sum.minerals || sum.minerals == 0) &&
                                    {gas >= sum.gas || sum.gas == 0} &&
                                    (supplyRemaining >= sum.supply || sum.supply == 0)
  def supplyRemaining = supply.available
  def -(sums: ResourceRequestSums) = {
    val supplyUpdated = supply.copy(supplyUsed + sums.supply)
    copy(minerals - sums.minerals, gas - sums.gas, supply = supplyUpdated)
  }
  def supplyUsed = supply.used
  def supplyUsagePercent = supply.supplyUsagePercent
}

class DefaultWorld(game: Game) extends WorldListener with WorldEventDispatcher {

  // these must be initialized after the first tick. making them lazy solves this
  lazy val mineralPatches = new MineralAnalyzer(map, units)
  lazy val strategicMap   = new StrategicMap(mineralPatches.resourceAreas, map.walkableGrid, game)

  val map        = new AnalyzedMap(game)
  val units      = new Units(game)
  val debugger   = new Debugger(game)
  val orderQueue = new OrderQueue(game, debugger)
  private var ticks = 0
  def nativeGame = game
  def currentResources = {
    val self = game.self()
    val total = self.supplyTotal()
    val used = self.supplyUsed()
    Resources(self.minerals(), self.gas(), Supplies(used, total))
  }

  def isFirstTick = ticks == 0

  def tickCount = ticks

  def tick(): Unit = {
    debugger.tick()
    units.tick()
  }

  def postTick(): Unit = {
    orderQueue.debugAll()
    orderQueue.issueAll()
    ticks += 1
  }
}

class Debugger(game: Game) {
  private val debugging  = true
  private val renderer   = new Renderer(game, Color.Green)
  private var countTicks = 0
  def faster(): Unit = {
    game.setLocalSpeed(3)
  }
  def debugRender(whatToDo: Renderer => Any): Unit = {
    countTicks += 1
    if (debugging && countTicks > 5) {
      whatToDo(renderer)
    }
  }
  def chat(msg: String): Unit = {
    game.sendText(msg)
  }
  def tick() = {
    renderer.in_!(Color.Green)
  }
  def revealMap(): Unit = {
    game.setRevealAll()
  }

}

class Units(game: Game) {
  private val knownUnits = mutable.HashMap.empty[Long, WrapsUnit]
  private var initial    = true
  def firstByType[T: Manifest]: Option[T] = {
    val lookFor = manifest[T].runtimeClass
    mine.find(lookFor.isInstance).map(_.asInstanceOf[T])
  }
  def mine = all.filter(_.nativeUnit.getPlayer == game.self())

  import scala.collection.JavaConverters._

  def mineByType[T: Manifest]: Iterator[T] = {
    val lookFor = manifest[T].runtimeClass
    mine.filter(lookFor.isInstance).map(_.asInstanceOf[T])
  }
  def minerals = allByType[MineralPatch]
  def allByType[T: Manifest]: Iterator[T] = {
    val lookFor = manifest[T].runtimeClass
    all.filter { e =>
      lookFor.isAssignableFrom(e.getClass)
    }.map(_.asInstanceOf[T])
  }
  def all = knownUnits.valuesIterator
  def tick(): Unit = {
    if (initial) {
      initial = false
      init()
    }
    game.self().getUnits.asScala.foreach {addUnit}
  }

  private def init(): Unit = {
    game.getMinerals.asScala.foreach {addUnit}
    game.getGeysers.asScala.foreach {addUnit}
  }

  private def addUnit(u: bwapi.Unit): Unit = {
    if (!knownUnits.contains(u.getID)) {
      val lifted = UnitWrapper.lift(u)
      info(s"Own unit added: $lifted")
      knownUnits.put(u.getID, lifted)
    }
  }
}


class Grid2D(val cols: Int, val rows: Int, areaDataBitSet: collection.Set[Int],
             protected val containsBlocked: Boolean = true) extends Serializable {
  self =>

  private val lazyAreas = LazyVal.from {new AreaHelper(self).findFreeAreas}
  def areaWhichContains(tile: MapTilePosition) = areas.find(_.containsAsData(tile))
  def areas = lazyAreas.get
  def includes(area: Area): Boolean = area.outline.forall(includes)
  def includes(p: MapTilePosition): Boolean = includes(p.x, p.y)
  def includes(x: Int, y: Int): Boolean = x >= 0 && x <= cols && y >= 0 && y <= rows
  def containsAsData(p: MapTilePosition): Boolean = !free(p)
  def free(p: MapTilePosition): Boolean = free(p.x, p.y)
  def connectedByLine(a: MapTilePosition, b: MapTilePosition) = AreaHelper.directLineOfSight(a, b, this)
  def blockedCount = size - freeCount
  def freeCount = if (containsBlocked) size - areaDataBitSet.size else areaDataBitSet.size
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
    val allBlockedIndexes = BitSet.empty ++ allIndexes.filter(blocked)
    new Grid2D(cols, rows, allBlockedIndexes)
  }
  def blocked(index: Int) = !free(index)
  def free(index: Int) = if (containsBlocked) !areaDataBitSet(index) else areaDataBitSet(index)
  def minAreaSize(i: Int) = {
    val mut = mutableCopy
    areas.filter(_.allContained.size < i).foreach { area =>
      area.allContained.foreach(mut.block_!)
    }
    mut.asReadOnly
  }
  def mutableCopy = new MutableGrid2D(cols, rows, mutable.BitSet.empty ++ areaDataBitSet)
  def allContained = allBlocked
  def allBlocked = if (containsBlocked) bitSetToTiles else allIndexes.filterNot(areaDataBitSet).map(indexToTile)
  def containedCount = areaDataBitSet.size
  def free(position: MapTilePosition, area: Size): Boolean = {
    area.points.forall { p =>
      free(p.movedBy(position))
    }
  }
  def zoomedOut = {
    val bits = mutable.BitSet.empty
    val subCols = cols / 4
    val subRows = rows / 4
    def squareFree(x: Int, y: Int) = free(x * 4, y * 4) && free(x * 4 + 1, y * 4) && free(x * 4, y * 4 + 1) &&
                                     free(x * 4 + 1, y * 4 + 1)
    for (x <- 0 until subCols; y <- 0 until subRows
         if !squareFree(x, y)) {
      bits += (x + y * subCols)
    }
    new Grid2D(subCols, subRows, bits)
  }
  override def toString: String = s"Grid2D(${areaDataBitSet.size} of ${size})"
  def size = cols * rows
  def blocked = size - walkable
  def walkable = areaDataBitSet.size
  def blocked(x: Int, y: Int): Boolean = !free(x, y)
  def blocked(p: MapTilePosition): Boolean = !free(p)
  def containsAsData(x: Int, y: Int): Boolean = !free(x, y)
  def free(x: Int, y: Int): Boolean = {
    val coord = x + y * cols
    if (containsBlocked) !areaDataBitSet(coord) else areaDataBitSet(coord)
  }
  def allFree = if (containsBlocked) allIndexes.filterNot(areaDataBitSet).map(indexToTile) else bitSetToTiles
  private def allIndexes = Iterator.range(0, size)
  private def indexToTile(index: Int) = MapTilePosition.shared(index % cols, index / rows)
  private def bitSetToTiles = areaDataBitSet.iterator.map(index => MapTilePosition.shared(index % cols, index / rows))
  def all: Iterator[MapTilePosition] = new Iterator[MapTilePosition] {
    private var index = 0
    private val max   = self.size
    override def hasNext = index < max
    override def next() = {
      val ret = MapTilePosition.shared(index % cols, index / rows)
      index += 1
      ret
    }
  }
  def areaCount = areas.size
}

class MutableGrid2D(cols: Int, rows: Int, bitSet: mutable.BitSet) extends Grid2D(cols, rows, bitSet) {

  def areaSize(anyContained: MapTilePosition) = {
    val isFree = free(anyContained)
    val on = if (isFree) this else reverseView
    AreaHelper.freeAreaSize(anyContained, on)
  }
  def reverseView: Grid2D = new Grid2D(cols, rows, bitSet, false)
  def anyFree = allFree.toStream.headOption

  override def areas = new AreaHelper(this).findFreeAreas

  def blockLine_!(a: MapTilePosition, b: MapTilePosition): Unit = {
    AreaHelper.traverseTilesOfLine(a, b, block_!)
  }
  def blockLineRelative_!(center: MapTilePosition, from: HasXY, to: HasXY): Unit = {
    val absoluteFrom = center.movedBy(from)
    val absoluteTo = center.movedBy(to)
    AreaHelper.traverseTilesOfLine(absoluteFrom, absoluteTo, block_!)
  }
  def asReadOnly: Grid2D = this
  def or_!(other: MutableGrid2D) = {
    assert(containsBlocked == other.containsBlocked)
    bitSet |= other.data
    this
  }
  def block_!(area: Area): Unit = {
    area.tiles.foreach { p => block_!(p.x, p.y) }
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
  protected def data = bitSet
}

class MineralAnalyzer(map: AnalyzedMap, myUnits: Units) {

  val groups        = {
    val patchGroups = ArrayBuffer.empty[MineralPatchGroup]
    val pf = new AreaHelper(map.walkableGrid)
    myUnits.minerals.foreach { mp =>
      patchGroups.find(g => !g.contains(mp) &&
                            g.center.distanceTo(mp.tilePosition) <= 10 &&
                            pf.directLineOfSight(mp.area, g.center)) match {
        case Some(group) => group.addPatch(mp)
        case None =>
          val newGroup = new MineralPatchGroup(patchGroups.size)
          newGroup.addPatch(mp)
          patchGroups += newGroup
      }
    }
    patchGroups.toSeq
  }
  val resourceAreas = {
    groups.map { patchGroup => ResourceArea(patchGroup, Set.empty) }
  }


  def nearestTo(position: MapTilePosition) = {
    if (groups.nonEmpty)
      Some(groups.minBy(_.center.distanceTo(position)))
    else
      None
  }

  info(
    s"""
       |Detected ${groups.size} mineral groups
     """.stripMargin)
}

case class MineralPatchGroup(patchId: Int) {
  private val myPatches = mutable.HashSet.empty[MineralPatch]
  private val myCenter  = new LazyVal[MapTilePosition](calcCenter)
  private val myValue   = new LazyVal[Int](myPatches.foldLeft(0)((acc, mp) => acc + mp.remaining))
  def tick() = {
    myValue.invalidate()
  }
  def addPatch(mp: MineralPatch): Unit = {
    myPatches += mp
    myCenter.invalidate()
  }

  override def toString = s"Minerals($value)@$center"
  def center = myCenter.get
  def value = myValue.get
  def patches = myPatches.toSet
  def contains(mp: MineralPatch): Boolean = myPatches(mp)
  private def calcCenter = {
    val (x, y) = myPatches.foldLeft((0, 0)) {
      case ((x, y), mp) => (x + mp.tilePosition.x, y + mp.tilePosition.y)
    }
    MapTilePosition.shared(x / myPatches.size, y / myPatches.size)
  }
}

class AnalyzedMap(game: Game) {
  val sizeX = game.mapWidth() * 4
  val sizeY = game.mapHeight() * 4

  val empty = new Grid2D(sizeX, sizeY, Set.empty)

  val walkableGridZoomed = {
    val bits = mutable.BitSet.empty
    0 until sizeX map { x =>
      0 until sizeY map { y =>
        if (!game.isWalkable(x, y)) {
          bits += x + (sizeX * y)
        }
      }
    }
    new Grid2D(sizeX, sizeY, bits).minAreaSize(6)
  }

  val buildableGrid = {
    val bits = mutable.BitSet.empty
    0 until sizeX / 4 map { x =>
      0 until sizeY / 4 map { y =>
        if (!game.isBuildable(x, y)) {
          bits += x + (sizeX / 4 * y)
        }
      }
    }
    new Grid2D(sizeX / 4, sizeY / 4, bits).minAreaSize(6)
  }

  val buildableGridZoomed = {
    // fake this for math reasons
    val bits = mutable.BitSet.empty
    0 until sizeX map { x =>
      0 until sizeY map { y =>
        if (!buildableGrid.free(x / 4, y / 4)) {
          bits += x + (sizeX * y)
        }
      }
    }
    new Grid2D(sizeX, sizeY, bits)
  }

  val walkableGrid = walkableGridZoomed.zoomedOut

  val areas = walkableGrid.areas

  def debugAreas = {
    val areas = walkableGrid.areas
    val debugThis = walkableGrid
    0 until debugThis.rows map { y =>
      0 until debugThis.cols map { x =>
        val index = areas.indexWhere(_.containsAsData(x, y))
        if (index == -1) " " else index.toString
      } mkString
    } mkString "\n"
  }

  def debugMap = walkableGrid.mkString('X')

  def debugMap2 = buildableGrid.mkString('X')

  info(
    s"""
       |Received map ${game.mapName()} with hash ${game.mapHash()}, size $sizeX * $sizeY
       |Total tiles ${walkableGridZoomed.size}
       |Walkable tiles ${walkableGridZoomed.walkable}
       |Blocked tiles ${walkableGridZoomed.blocked}
       |Walkable map
       |$debugMap
       |Buildable map
       |$debugMap2
       |Area analysis
       |$debugAreas
     """.stripMargin)
}

object DefaultWorld {
  def spawn(game: Game) = new DefaultWorld(game)
}