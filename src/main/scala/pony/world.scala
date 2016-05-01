package pony

import bwapi.{Color, Game, PlayerType, Player => BWPlayer}
import pony.brain.modules.GroupingHelper
import pony.brain.{HasUniverse, ResourceRequestSum, Supplies, Universe}

import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class UnitData(in: bwapi.Unit) {
  // todo use this
}

case class Resources(minerals: Int, gas: Int, supply: Supplies) {
  val asSum = ResourceRequestSum(minerals, gas, supply.available)

  def moreGasThanMinerals = minerals < gas

  def >(min: Int, gas: Int, supply: Int) = {
    minerals >= min && this.gas >= gas && this.supplyRemaining >= supply
  }

  def supplyRemaining = supply.available

  def moreMineralsThanGas = minerals > gas * 1.5

  def supplyTotal = supply.total

  def -(sums: ResourceRequestSum) = {
    val supplyUpdated = supply.copy(supplyUsed + sums.supply)
    copy(minerals - sums.minerals, gas - sums.gas, supply = supplyUpdated)
  }

  def supplyUsed = supply.used

  def supplyUsagePercent = supply.supplyUsagePercent
}

case class AllUnits(own: Units, other: Units) {
  def byNativeId(id: Int) = {
    own.byId(id).orElse(other.byId(id))
  }
}

class DefaultWorld(game: Game) extends WorldListener with WorldEventDispatcher {
  private var myUniverse: Universe = _

  def init_!(universe: Universe) = {
    this.myUniverse = universe
  }

  def universe = myUniverse

  def addPostTickAction(value: => Unit) = {
    postTickActions += (() => value)
  }

  // these must be initialized after the first tick. making them lazy solves this
  lazy val resourceAnalyzer = new ResourceAnalyzer(map,
    AllUnits(myUniverse.ownUnits, myUniverse.enemyUnits))
  lazy val strategicMap     = new
      StrategicMap(resourceAnalyzer.resourceAreas, map.walkableGrid, game)

  val map        = new AnalyzedMap(game)
  val debugger   = new Debugger(game, this)
  val orderQueue = new OrderQueue(game, debugger)
  private val removeQueueOwn   = ArrayBuffer.empty[bwapi.Unit]
  private val removeQueueEnemy = ArrayBuffer.empty[bwapi.Unit]
  private var ticks            = 0
  private val postTickActions  = ArrayBuffer.empty[() => Unit]

  def nativeGame = game

  def currentResources = {
    val self = game.self()
    val total = self.supplyTotal()
    val used = self.supplyUsed()
    Resources(self.minerals(), self.gas(), Supplies(used, total))
  }

  def isFirstTick = ticks == 0

  def tickCount = ticks

  override def onUnitDestroy(unit: bwapi.Unit): Unit = {
    super.onUnitDestroy(unit)
    if (unit.getPlayer.isEnemy(game.self())) {
      removeQueueEnemy += unit
    } else {
      removeQueueOwn += unit
    }
  }

  def tick(): Unit = {
    myUniverse.ownUnits.dead_!(removeQueueOwn)
    myUniverse.enemyUnits.dead_!(removeQueueEnemy)
    removeQueueOwn.clear()
    removeQueueEnemy.clear()
    myUniverse.ownUnits.tick()
    myUniverse.enemyUnits.tick()
  }

  def postTick(): Unit = {
    debugger.renderer.allow()
    postTickActions.foreach(e => e())
    debugger.renderer.disallow()
    postTickActions.clear()
    orderQueue.debugAll()
    orderQueue.issueAll()
    ticks += 1
  }
}

class Debugger(game: Game, world: DefaultWorld) {
  def isRendering = logLevel.includes(LogLevels.LogTrace)

  val renderer = new Renderer(game, Color.Green)
  private var debugging     = true
  private var fullDebugMode = false
  private var countTicks    = 0

  def isFullDebug = fullDebugMode

  def off(): Unit = {
    debugging = false
  }

  def fullOn(): Unit = {
    on()
    fullDebugMode = true
  }

  def on(): Unit = {
    debugging = true
  }

  def fullOff(): Unit = {
    fullDebugMode = false
  }

  def isDebugging = debugging || fullDebugMode

  def speed(int: Int): Unit = {
    game.setLocalSpeed(int)
  }

  def fastest(): Unit = {
    game.setLocalSpeed(0)
  }

  def slowMotion(): Unit = {
    game.setLocalSpeed(255)
  }

  def debugRender(whatToDo: Renderer => Any): Unit = {
    countTicks += 1
    if (debugging && countTicks > 5 && isRendering) {
      world.addPostTickAction {
        whatToDo(renderer)
      }
    }
  }

  def chat(msg: String): Unit = {
    game.sendText(msg)
  }

  def revealMap(): Unit = {
    game.setRevealAll()
  }

}

object OnKillListener {
  def on[T <: WrapsUnit, X](unit: T, doThis: () => X) = new OnKillListener[T](unit) {
    override def onKill(t: T): Unit = {
      assert(t == unit)
      doThis()
    }
  }
}

abstract class OnKillListener[T <: WrapsUnit](val unit: T) {
  def onKill(t: T)

  def onKillUnTyped(t: WrapsUnit) = {
    assert(unit == t)
    onKill(unit)
  }

  def nativeUnitId = unit.nativeUnitId
}

class Units(game: Game, hostile: Boolean, override val universe: Universe) extends HasUniverse {
  private val killListeners      = mutable.HashMap.empty[Int, OnKillListener[_]]
  private val killListenersOnAll = mutable.ArrayBuffer.empty[WrapsUnit => Unit]
  private val newUnitListeners   = mutable.ArrayBuffer.empty[WrapsUnit => Unit]
  private val fresh              = ArrayBuffer.empty[WrapsUnit]
  private val graveyard          = mutable.HashMap.empty[Int, WrapsUnit]
  private val classIndexes       = mutable.HashSet.empty[Class[_]]
  private val preparedByClass    = multiMap[Class[_], WrapsUnit]
  private val nativeIdToUnit     = mutable.HashMap.empty[Int, WrapsUnit]

  private var initial            = true

  def byNative(nativeUnit: bwapi.Unit) = if (nativeUnit == null) None else byId(nativeUnit.getID)

  def byId(nativeId: Int) = nativeIdToUnit.get(nativeId)

  def byIdExpectExisting(nativeId: Int) = nativeIdToUnit(nativeId)

  def consumeFresh_![X](f: WrapsUnit => X) = {
    fresh.foreach(f)
    fresh.clear()
  }

  def registerKill_![T <: WrapsUnit](listener: OnKillListener[T]): Unit = {
    // this will always replace the latest listener
    killListeners += ((listener.nativeUnitId, listener))
  }

  def registerKill_!(listener: WrapsUnit => Unit): Unit = {
    killListenersOnAll += listener
  }

  def registerAdd_!(listener: WrapsUnit => Unit): Unit = {
    newUnitListeners += listener
  }

  def dead_!(dead: Seq[bwapi.Unit]) = {
    dead.foreach { u =>
      nativeIdToUnit.get(u.getID).foreach { died =>
        died match {
          case cd: CanDie =>
            cd.notifyDead_!()
          case _ =>
        }
        killListeners.get(u.getID).foreach { e =>
          e.onKillUnTyped(died)
          killListeners.remove(u.getID)
        }
        killListenersOnAll.foreach {_.apply(died)}
        val nowDead = removeUnit(u)
        nowDead.foreach {
          case cd: CanDie => cd.notifyDead_!()
          case _ =>
        }
        nowDead.foreach { e =>
          graveyard += ((u.getID, e))
        }
      }
    }
  }

  private def removeUnit(u: bwapi.Unit) = {
    val removed = nativeIdToUnit.remove(u.getID)
    removed.foreach { what =>
      classIndexes.foreach { c =>
        preparedByClass.removeBinding(c, what)
      }
    }
    removed.foreach(_.notifyRemoved_!())
    removed
  }

  def buildingAt(upperLeft: MapTilePosition) = {
    allBuildings.find(_.area.upperLeft == upperLeft)
  }

  def allBuildings = allByType[Building]

  def allBuildingsWithWeapons = allByType[ArmedBuildingCoveringGroundAndAir]

  def allBuildingsWithGroundWeapons = allByType[ArmedBuildingCoveringGround]

  def allBuildingsWithAirWeapons = allByType[ArmedBuildingCoveringAir]

  def allDetectors = allByType[Detector]

  def allCompletedMobiles = allMobiles.filterNot(_.isBeingCreated)

  def allWithGroundWeapon = allByType[GroundWeapon]

  def allMobilesWithWeapons = allByType[ArmedMobile]

  def allWithAirWeapon = allByType[AirWeapon]

  def allMobiles = allByType[Mobile]

  def allAddonBuilders = allByType[CanBuildAddons]

  def allAddons = allByType[Addon]

  def existsIncomplete(c: Class[_ <: WrapsUnit]) = allByClass(c).exists(_.isBeingCreated)

  def existsComplete(c: Class[_ <: WrapsUnit]) = allByClass(c).exists(!_.isBeingCreated)

  def ownsByType(c: Class[_ <: WrapsUnit]) = {
    nativeIdToUnit.values.exists(c.isInstance)
  }

  def geysirs = allByType[Geysir]

  def allByType[T <: WrapsUnit : Manifest] = {
    val lookFor = manifest[T].runtimeClass.asInstanceOf[Class[T]]
    allByClass(lookFor)
  }

  def allByClass[T <: WrapsUnit](lookFor: Class[T]) = {
    def lazyCreate = {
      classIndexes += lookFor
      mutable.HashSet.empty ++= allKnownUnits.filter { e => lookFor.isInstance(e) }
    }
    val cached = preparedByClass.getOrElseUpdate(lookFor, lazyCreate)
    cached.asInstanceOf[collection.Set[T]]
  }

  def allKnownUnits = nativeIdToUnit.valuesIterator

  def allRelevant = nativeIdToUnit.valuesIterator.filterNot(_.isInstanceOf[Irrelevant])

  def allCanDie = allByType[CanDie]

  import scala.collection.JavaConverters._

  def firstByType[T: Manifest]: Option[T] = {
    val lookFor = manifest[T].runtimeClass
    inFaction.find(lookFor.isInstance).map(_.asInstanceOf[T])
  }

  def inFaction = allKnownUnits.filter(_.nativeUnit.getPlayer == game.self())

  def minerals = allByType[MineralPatch]

  def allMobilesAndBuildings = allCompletedMobiles ++ allBuildings

  def tick(): Unit = {
    if (initial) {
      initial = false
      init()
    }
    //sometimes units die without the event being triggered
    if (universe.currentTick % 19 == 0) {
      val dead = allRelevant.filterNot(_.isInGame).map { e =>
        warn(s"Unit $e died without event")
        e.nativeUnit
      }.toSeq
      dead_!(dead)
    }


    val addThese = {
      if (ownAndNeutral)
        game.self().getUnits.asScala
      else
        game.enemies().asScala.flatMap(_.getUnits.asScala)
    }
    addThese.foreach {addUnit}
  }

  def registerUnit(u: bwapi.Unit, lifted: WrapsUnit) = {
    newUnitListeners.foreach {_.apply(lifted)}
    nativeIdToUnit.put(u.getID, lifted)
    classIndexes.foreach { c =>
      if (c.isInstance(lifted)) {
        preparedByClass.addBinding(c, lifted)
      }
    }
  }

  private def init(): Unit = {
    if (ownAndNeutral) {
      game.getMinerals.asScala.foreach {addUnit}
      game.getGeysers.asScala.foreach {addUnit}
    }
  }

  private def addUnit(u: bwapi.Unit): Unit = {
    val record = {
      if (ownAndNeutral) {
        forces.isNotEnemy(u)
      } else {
        forces.isEnemy(u)
      }
    }
    if (record) {
      if (!graveyard.contains(u.getID)) {
        nativeIdToUnit.get(u.getID) match {
          case None =>
            val lifted = UnitWrapper.lift(u)
            fresh += lifted
            info(s"${ownAndNeutral.ifElse("Own", "Hostile")} unit added: $lifted")
            registerUnit(u, lifted)
          case Some(unit) if unit.initialNativeType != u.getType =>
            if (unit.shouldReRegisterOnMorph) {
              info(s"Unit morphed from ${unit.initialNativeType} to ${u.getType}")
              val lifted = UnitWrapper.lift(u)

              // clean up old indexes
              classIndexes.foreach { c =>
                if (c.isInstance(unit)) {
                  preparedByClass.removeBinding(c, unit)
                }
              }


              registerUnit(u, lifted)
              fresh += lifted
              unit.onMorph(u.getType)
            }
          case _ => // noop
        }
      } else {
        warn("zombie?")
      }
    }
  }

  private def ownAndNeutral = !hostile


}

class Forces(me: bwapi.Player, myAllies: Set[bwapi.Player], myEnemies: Set[bwapi.Player],
             override val universe: Universe) extends HasUniverse {

  def myRace = myself.scRace

  def isTvT = {
    forces.myself.scRace.isTerran &&
    forces.hostilePlayers.size == 1 &&
    forces.hostilePlayers.players.head.scRace.isTerran
  }

  def isTvZ = {
    forces.myself.scRace.isTerran &&
    forces.hostilePlayers.size == 1 &&
    forces.hostilePlayers.players.head.scRace.isZerg
  }

  def isTvP = {
    forces.myself.scRace.isTerran &&
    forces.hostilePlayers.size == 1 &&
    forces.hostilePlayers.players.head.scRace.isProtoss
  }

  val myself         = Player(me)(universe)
  val alliedPlayers  = Force(myAllies.map(Player(_)(universe)))(universe)
  val hostilePlayers = Force(myEnemies.map(Player(_)(universe)))(universe)

  def isNotEnemy(u: bwapi.Unit) = !isEnemy(u)

  def isEnemy(u: bwapi.Unit) = !isNeutral(u) && !isFriend(u)

  def isFriend(u: bwapi.Unit) = {
    u.getPlayer == me || myAllies(u.getPlayer)
  }

  def isNeutral(u: bwapi.Unit) = u.getPlayer.getType == PlayerType.None
}

case class Player(base: bwapi.Player)(override val universe: Universe) extends HasUniverse {
  val nativeRace = base.getRace
  val scRace     = SCRace.fromNative(nativeRace)
}

case class Force(players: Set[Player])(override val universe: Universe) extends HasUniverse {
  def size = players.size
}

case class SerializablePatchGroup(areas: Seq[Area])

class ResourceAnalyzer(map: AnalyzedMap, all: AllUnits) {

  lazy val groups        = myGroups.zipWithIndex.map { case (serializable, index) =>
    val pg = new MineralPatchGroup(index)
    serializable.areas.foreach { e =>
      val minerals = myUnits.minerals.find(_.area == e).getOr(s"Could not find minerals at $e")
      pg.addPatch(minerals)
    }
    pg
  }
  lazy val resourceAreas = {
    val mineralBased = groups.map { patchGroup =>
      val geysirs = myUnits.geysirs
                    .filter { e =>
                      val close = e.area.distanceTo(patchGroup.center) < 20
                      def sameArea = patchGroup.patches.exists { p =>
                        map.walkableGrid.areInSameWalkableArea(e.centerTile, p.centerTile)
                      }
                      close && sameArea
                    }
                    .toSet

      ResourceArea(Some(patchGroup), geysirs)
    }
    val allAreas = mineralBased ++ {
      val lonelyGeysirs = myUnits.geysirs
                          .filterNot { geysir =>
                            mineralBased.exists(_.geysirs(geysir))
                          }
      val groups = GroupingHelper.groupTheseNow(lonelyGeysirs, map.walkableGrid, all)
      groups.map { group =>
        ResourceArea(None, group.memberUnits.toSet)
      }
    }
    allAreas.toVector
  }

  val myUnits = all.own
  private val myGroups      = FileStorageLazyVal.fromFunction({
    info(s"Calculating mineral groups...")
    val patchGroups = ArrayBuffer.empty[MineralPatchGroup]
    val allMins = ArrayBuffer.empty ++= myUnits.minerals
    val pathFinder = PathFinder.on(map.walkableGrid, isOnGround = true)
    allMins.foreach { mp =>
      print(".")
      patchGroups.find { g =>
        def isNew = !g.contains(mp)
        def isClose = {
          g.allTiles.exists { check =>
            val path = pathFinder.findSimplePathNow(check, mp.tilePosition, tryFixPath = false)
            path.exists { p =>
              p.isPerfectSolution && p.length < 20
            }
          }
        }
        isNew && isClose
      } match {
        case Some(group) => group.addPatch(mp)
        case None =>
          val newGroup = new MineralPatchGroup(patchGroups.size)
          newGroup.addPatch(mp)
          patchGroups += newGroup
      }
    }

    patchGroups.map(e => SerializablePatchGroup(e.patches.map(_.area).toSeq))
  }, s"mineralgroups_${map.game.suggestFileName}")

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

  def anyTile = allTiles.next()

  private val myPatches      = mutable.HashSet.empty[MineralPatch]
  private val myCenter       = new LazyVal[MapTilePosition](calcCenter)
  private val myValue        = new
      LazyVal[Int](myPatches.foldLeft(0)((acc, mp) => acc + mp.remaining))
  private val myInitialValue = LazyVal.from(myPatches.foldLeft(0)((acc, mp) => acc + mp.remaining))

  def allTiles = myPatches.iterator.flatMap(_.area.tiles)

  def tick() = {
    myValue.invalidate()
  }

  def addPatch(mp: MineralPatch): Unit = {
    myPatches += mp
    myCenter.invalidate()
    myInitialValue.invalidate()
  }

  def remainingPercentage = myValue / myInitialValue.toDouble

  override def toString = s"Minerals#$patchId($value)@$center"

  def center = myCenter.get

  def value = myValue.get

  def patches = myPatches.toSet

  def contains(mp: MineralPatch): Boolean = myPatches(mp)

  private def calcCenter = {
    val (x, y) = myPatches.foldLeft((0, 0)) {
      case ((x, y), mp) => (x + mp.tilePosition.x, y + mp.tilePosition.y)
    }
    val reference = MapTilePosition.shared(x / myPatches.size, y / myPatches.size)
    myPatches.flatMap(_.area.tiles).minBy(_.distanceSquaredTo(reference))
  }
}

class AnalyzedMap(val game: Game) {

  val sizeX = game.mapWidth() * 4
  val sizeY = game.mapHeight() * 4

  val tileSizeX = game.mapWidth()
  val tileSizeY = game.mapHeight()

  val empty       = new Grid2D(sizeX, sizeY, BitSet.empty)
  val emptyZoomed = empty.zoomedOut

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
    val encoded = ('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') ++ (1 to 100 map (_ => '?'))
    val areas = walkableGrid.areas
    val separated = areas.map(_.mkString('X')).mkString("\n---\n")
    val debugThis = walkableGrid
    separated + "\n" + (0 until debugThis.rows map { y =>
      0 until debugThis.cols map { x =>
        val index = areas.indexWhere(_.free(x, y))
        if (index == -1) " " else encoded(index).toString
      } mkString
    } mkString "\n")
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
