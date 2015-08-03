package pony

import bwapi.{Color, Game, Player, Position}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class UnitData(in: bwapi.Unit) {

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

case class Resources(minerals: Int, gas: Int, supply: Int, supplyUsed: Int, supplyTotal: Int)

class DefaultWorld(game: Game) extends WorldListener {

  // these must be initialized after the first tick. making them lazy solves this
  lazy val mineralPatches = new MineralAnalyzer(map, units)
  val map        = new AnalyzedMap(game)
  val units      = new Units(game)
  val debugger   = new Debugger(game)
  val orderQueue = new OrderQueue(game, debugger)
  private var ticks = 0
  def currentResources = {
    val self = game.self()
    val total = self.supplyTotal()
    val used = self.supplyUsed()
    Resources(self.minerals(), self.gas(), total - used, used, total)
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
  private val debugging = true
  private val renderer  = new Renderer(game, Color.Green)
  def debugRender(whatToDo: Renderer => Any): Unit = {
    if (debugging) {
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

  def all = knownUnits.valuesIterator

  def mineByType[T: Manifest]: Iterator[T] = {
    val lookFor = manifest[T].runtimeClass
    mine.filter(lookFor.isInstance).map(_.asInstanceOf[T])
  }

  def allByType[T: Manifest]: Iterator[T] = {
    val lookFor = manifest[T].runtimeClass
    all.filter(lookFor.isInstance).map(_.asInstanceOf[T])
  }

  def minerals = knownUnits.valuesIterator.collect { case u: MineralPatch => u }
  def tick(): Unit = {
    if (initial) {
      initial = false
      init()
    }
    game.self().getUnits.asScala.foreach {addUnit}
  }
  private def init(): Unit = {
    game.getMinerals.asScala.foreach {addUnit}
  }
  private def addUnit(u: bwapi.Unit): Unit = {
    if (!knownUnits.contains(u.getID)) {
      val lifted = UnitWrapper.lift(u)
      info(s"Own unit added: ${lifted}")
      knownUnits.put(u.getID, lifted)
    }
  }
}

class Grid2D(val cols: Int, val rows: Int, bitset: collection.Set[Int]) {
  def zoomedOut = {
    val bits = mutable.BitSet.empty
    val subCols = cols / 4
    val subRows = rows / 4
    def squareFree(x: Int, y: Int) = free(x * 4, y * 4) && free(x * 4 + 1, y * 4) && free(x * 4, y * 4 + 1) &&
                                     free(x * 4 + 1, y * 4 + 1)
    for (x <- 0 until subCols; y <- 0 until subRows
         if squareFree(x, y)) {
      bits += (x + y * subCols)
    }
    new Grid2D(subCols, subRows, bits)
  }
  def blocked = size - walkable
  def size = cols * rows
  def walkable = bitset.size
  def blocked(x: Int, y: Int): Boolean = !free(x, y)
  def blocked(p: MapTilePosition): Boolean = !free(p)
  def free(p: MapTilePosition): Boolean = free(p.x, p.y)
  def free(x: Int, y: Int): Boolean = bitset(x + y * cols)
  def all = new Traversable[MapTilePosition] {
    override def foreach[U](f: (MapTilePosition) => U): Unit = {
      for (x <- 0 until cols; y <- 0 until rows) {
        f(MapTilePosition.shared(x, y))
      }
    }
  }
}

class MineralAnalyzer(map: AnalyzedMap, myUnits: Units) {
  val groups = {
    val patchGroups = ArrayBuffer.empty[MineralPatchGroup]
    val pf = new SimplePathFinder(map.walkableGrid)
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

class MineralPatchGroup(val patchId: Int) {
  private val myPatches = mutable.HashSet.empty[MineralPatch]
  private val myCenter  = new LazyVal[MapTilePosition](calcCenter)
  private val myValue   = new LazyVal[Int](myPatches.foldLeft(0)((acc, mp) => acc + mp.remaining))
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

  val walkableGridZoomed = {
    val bits = mutable.BitSet.empty
    0 until sizeX map { x =>
      0 until sizeY map { y =>
        if (game.isWalkable(x, y)) {
          bits += x + (sizeX * y)
        }
      }
    }
    new Grid2D(sizeX, sizeY, bits)
  }

  val walkableGrid = walkableGridZoomed.zoomedOut

  def debugMap = {
    val debugThis = walkableGrid
    0 until debugThis.rows map { x =>
      0 until debugThis.cols map { y =>
        if (debugThis.free(x, y)) " " else "X"
      } mkString
    } mkString "\n"
  }

  info(
    s"""
       |Received map ${game.mapName()} with hash ${game.mapHash()}, size $sizeX * $sizeY
       |Total tiles ${walkableGridZoomed.size}
       |Walkable tiles ${walkableGridZoomed.walkable}
       |Blocked tiles ${walkableGridZoomed.blocked}
       |Map: $debugMap
     """.stripMargin)
}

object DefaultWorld {
  def spawn(game: Game) = new DefaultWorld(game)
}