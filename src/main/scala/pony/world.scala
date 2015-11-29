package pony

import scala.collection.JavaConverters._
import scala.collection.immutable.BitSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

import bwapi.{Color, Game, Player, PlayerType, Position, TilePosition}
import pony.brain.{ResourceRequestSum, Supplies}

class UnitData(in: bwapi.Unit) {
  // todo use this
}

trait TechTree {
  lazy    val requiredBy        = {
    val result = multiMap[Class[_ <: WrapsUnit], Class[_ <: WrapsUnit]]
    dependsOn.foreach { case (target, needs) =>
      needs.foreach { c =>
        result.addBinding(c, target)
      }
    }
    result.toImmutable
  }
  protected val builtBy  : Map[Class[_ <: WrapsUnit], Class[_ <: WrapsUnit]]
  protected val dependsOn: Map[Class[_ <: WrapsUnit], Set[_ <: Class[_ <: Building]]]
  protected val upgrades : Map[Upgrade, Class[_ <: Upgrader]]
  private val requirementsCache = mutable.HashMap.empty[Class[_ <: WrapsUnit], Set[Class[_ <: Building]]]
  def mainBuildingOf(addon: Class[_ <: Addon]) = {
    builtBy(addon).asInstanceOf[Class[_ <: CanBuildAddons]]
  }
  def upgraderFor(upgrade: Upgrade) = upgrades(upgrade)

  def canBuild(factory: Class[_ <: UnitFactory], mobile: Class[_ <: Mobile]) = {
    builtBy(mobile) == factory
  }
  def canUpgrade(u: Class[_ <: Upgrader], up: Upgrade) = upgrades(up) == u
  def canBuildAddon(main: Class[_ <: CanBuildAddons], addon: Class[_ <: Addon]) = {
    builtBy(addon) == main
  }
  def requiredFor[T <: WrapsUnit](what: Class[_ <: T]) = {
    requirementsCache.getOrElseUpdate(what, {
      val all = mutable.Set.empty[Class[_ <: Building]]
      var head = mutable.Set.empty[Class[_ <: Building]] ++= dependsOn.getOrElse(what, Set.empty)
      var goOn = true
      do {
        all ++= head
        head = head.flatMap(e => dependsOn.getOrElse(e, Set.empty))
      }
      while (head.nonEmpty)
      all.toSet
    })
  }
}

class TerranTechTree extends TechTree {

  override protected val upgrades: Map[Upgrade, Class[_ <: Upgrader]] = Map(
    Upgrades.Terran.GoliathRange -> classOf[MachineShop],
    Upgrades.Terran.CruiserGun -> classOf[PhysicsLab],
    Upgrades.Terran.CruiserEnergy -> classOf[PhysicsLab],
    Upgrades.Terran.Irradiate -> classOf[ScienceFacility],
    Upgrades.Terran.EMP -> classOf[ScienceFacility],
    Upgrades.Terran.ScienceVesselEnergy -> classOf[ScienceFacility],
    Upgrades.Terran.GhostCloak -> classOf[CovertOps],
    Upgrades.Terran.GhostEnergy -> classOf[CovertOps],
    Upgrades.Terran.GhostStop -> classOf[CovertOps],
    Upgrades.Terran.GhostVisiblityRange -> classOf[CovertOps],
    Upgrades.Terran.TankSiegeMode -> classOf[MachineShop],
    Upgrades.Terran.VultureSpeed -> classOf[MachineShop],
    Upgrades.Terran.SpiderMines -> classOf[MachineShop],
    Upgrades.Terran.WraithCloak -> classOf[ControlTower],
    Upgrades.Terran.WraithEnergy -> classOf[ControlTower],
    Upgrades.Terran.MarineRange -> classOf[Academy],
    Upgrades.Terran.MedicEnergy -> classOf[Academy],
    Upgrades.Terran.MedicHeal -> classOf[Academy],
    Upgrades.Terran.MedicFlare -> classOf[Academy],
    Upgrades.Terran.InfantryCooldown -> classOf[Academy],
    Upgrades.Terran.InfantryArmor -> classOf[EngineeringBay],
    Upgrades.Terran.InfantryWeapons -> classOf[EngineeringBay],
    Upgrades.Terran.VehicleWeapons -> classOf[Armory],
    Upgrades.Terran.VehicleArmor -> classOf[Armory],
    Upgrades.Terran.ShipArmor -> classOf[Armory],
    Upgrades.Terran.ShipWeapons -> classOf[Armory]

  )

  override protected val builtBy: Map[Class[_ <: WrapsUnit], Class[_ <: Building]] = Map(
    classOf[Comsat] -> classOf[CommandCenter],
    classOf[NuclearSilo] -> classOf[CommandCenter],
    classOf[Dropship] -> classOf[Starport],
    classOf[MachineShop] -> classOf[Factory],
    classOf[ControlTower] -> classOf[Starport],
    classOf[PhysicsLab] -> classOf[ScienceFacility],
    classOf[CovertOps] -> classOf[ScienceFacility],
    classOf[SCV] -> classOf[CommandCenter],
    classOf[Marine] -> classOf[Barracks],
    classOf[Firebat] -> classOf[Barracks],
    classOf[Ghost] -> classOf[Barracks],
    classOf[Medic] -> classOf[Barracks],
    classOf[Vulture] -> classOf[Factory],
    classOf[Tank] -> classOf[Factory],
    classOf[Goliath] -> classOf[Factory],
    classOf[Wraith] -> classOf[Starport],
    classOf[Battlecruiser] -> classOf[Starport],
    classOf[ScienceVessel] -> classOf[Starport]
  )

  override protected val dependsOn: Map[Class[_ <: WrapsUnit], Set[_ <: Class[_ <: Building]]] = {
    val inferred = builtBy.map { case (k, v) => k -> Set(v) }.toList

    val additional: Map[Class[_ <: WrapsUnit], Set[_ <: Class[_ <: Building]]] = Map(
      classOf[Factory] -> classOf[Barracks].toSet,
      classOf[Comsat] -> Set(classOf[CommandCenter], classOf[Academy]),
      classOf[Starport] -> classOf[Factory].toSet,
      classOf[ScienceFacility] -> classOf[Starport].toSet,
      classOf[ControlTower] -> classOf[Starport].toSet,
      classOf[MissileTurret] -> classOf[EngineeringBay].toSet,
      classOf[Ghost] -> classOf[CovertOps].toSet,
      classOf[Bunker] -> classOf[Barracks].toSet,
      classOf[Firebat] -> classOf[Academy].toSet,
      classOf[Medic] -> classOf[Academy].toSet,
      classOf[Tank] -> classOf[MachineShop].toSet,
      classOf[Goliath] -> Set(classOf[MachineShop], classOf[Armory]),
      classOf[Battlecruiser] -> Set(classOf[ControlTower], classOf[PhysicsLab]),
      classOf[ScienceVessel] -> Set(classOf[ScienceFacility], classOf[ControlTower]),
      classOf[Valkery] -> Set(classOf[ControlTower], classOf[Armory])
    )

    (inferred ++ additional.toList).groupBy(_._1).map { case (k, vs) => k -> vs.flatMap(_._2).toSet }

  }
}

sealed trait SCRace {
  val techTree: TechTree
  def specialize[T](unitType: Class[_ <: T]) = {
    (if (classOf[WorkerUnit].isAssignableFrom(unitType)) {
      workerClass
    } else
    if (classOf[ResourceGatherPoint].isAssignableFrom(unitType)) {
      resourceDepositClass
    } else
    if (classOf[MainBuilding].isAssignableFrom(unitType)) {
      resourceDepositClass
    } else
    if (classOf[SupplyProvider].isAssignableFrom(unitType)) {
      supplyClass
    } else
    if (classOf[TransporterUnit].isAssignableFrom(unitType)) {
      transporterClass
    } else
      unitType).asInstanceOf[Class[_ <: T]]
  }
  def resourceDepositClass: Class[_ <: MainBuilding]
  def workerClass: Class[_ <: WorkerUnit]
  def transporterClass: Class[_ <: TransporterUnit]
  def supplyClass: Class[_ <: SupplyProvider]
}

case object Terran extends SCRace {
  override val techTree = new TerranTechTree
  override def workerClass: Class[_ <: WorkerUnit] = classOf[SCV]
  override def transporterClass: Class[_ <: TransporterUnit] = classOf[Dropship]
  override def supplyClass: Class[_ <: SupplyProvider] = classOf[SupplyDepot]
  override def resourceDepositClass: Class[_ <: MainBuilding] = classOf[CommandCenter]
}

case object Zerg extends SCRace {
  override val techTree = ???
  override def workerClass: Class[_ <: WorkerUnit] = classOf[Drone]
  override def transporterClass: Class[_ <: TransporterUnit] = classOf[Overlord]
  override def supplyClass: Class[_ <: SupplyProvider] = classOf[Overlord]
  override def resourceDepositClass: Class[_ <: MainBuilding] = classOf[Hive]
}

case object Protoss extends SCRace {
  override val techTree = ???
  override def workerClass: Class[_ <: WorkerUnit] = classOf[Probe]
  override def transporterClass: Class[_ <: TransporterUnit] = classOf[Shuttle]
  override def supplyClass: Class[_ <: SupplyProvider] = classOf[Pylon]
  override def resourceDepositClass: Class[_ <: MainBuilding] = classOf[Nexus]
}

case object Other extends SCRace {
  override val techTree: TechTree = ???
  override def workerClass: Class[_ <: WorkerUnit] = ???
  override def transporterClass: Class[_ <: TransporterUnit] = ???
  override def supplyClass: Class[_ <: SupplyProvider] = ???
  override def resourceDepositClass: Class[_ <: MainBuilding] = ???
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

class DefaultWorld(game: Game) extends WorldListener with WorldEventDispatcher {

  // these must be initialized after the first tick. making them lazy solves this
  lazy val resourceAnalyzer = new ResourceAnalyzer(map, ownUnits)
  lazy val strategicMap     = new StrategicMap(resourceAnalyzer.resourceAreas, map.walkableGrid, game)

  val map        = new AnalyzedMap(game)
  val ownUnits   = new Units(game, false)
  val enemyUnits = new Units(game, true)
  val debugger   = new Debugger(game)
  val orderQueue = new OrderQueue(game, debugger)
  private val removeQueueOwn   = ArrayBuffer.empty[bwapi.Unit]
  private val removeQueueEnemy = ArrayBuffer.empty[bwapi.Unit]
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
  override def onUnitDestroy(unit: bwapi.Unit): Unit = {
    super.onUnitDestroy(unit)
    if (unit.getPlayer.isEnemy(game.self())) {
      removeQueueEnemy += unit
    } else {
      removeQueueOwn += unit
    }
  }

  def tick(): Unit = {
    debugger.tick()
    ownUnits.dead_!(removeQueueOwn.toSeq)
    enemyUnits.dead_!(removeQueueEnemy.toSeq)
    removeQueueOwn.clear()
    removeQueueEnemy.clear()
    ownUnits.tick()
    enemyUnits.tick()
  }

  def postTick(): Unit = {
    orderQueue.debugAll()
    orderQueue.issueAll()
    ticks += 1
  }
}

class Debugger(game: Game) {
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

class Units(game: Game, hostile: Boolean) {
  private val killListeners      = mutable.HashMap.empty[Int, OnKillListener[_]]
  private val killListenersOnAll = mutable.ArrayBuffer.empty[WrapsUnit => Unit]
  private val newUnitListeners   = mutable.ArrayBuffer.empty[WrapsUnit => Unit]
  private val fresh              = ArrayBuffer.empty[WrapsUnit]
  private val graveyard          = mutable.HashMap.empty[Int, WrapsUnit]
  private val classIndexes       = mutable.HashSet.empty[Class[_]]
  private val preparedByClass    = multiMap[Class[_], WrapsUnit]
  private val nativeIdToUnit     = mutable.HashMap.empty[Int, WrapsUnit]
  private val forces             = LazyVal.from {
    val me = game.self
    val friends = game.allies()
    Forces(me, friends.asScala.toSet)
  }
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
          case cd: MaybeCanDie =>
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
          case cd: MaybeCanDie => cd.notifyDead_!()
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
  def allCompletedMobiles = allMobiles.filterNot(_.isBeingCreated)
  def allMobiles = allByType[Mobile]
  def allAddonBuilders = allByType[CanBuildAddons]
  def allAddons = allByType[Addon]
  def existsIncomplete(c: Class[_ <: WrapsUnit]) = allByClass(c).exists(_.isBeingCreated)
  def existsComplete(c: Class[_ <: WrapsUnit]) = allByClass(c).exists(!_.isBeingCreated)
  def ownsByType(c: Class[_ <: WrapsUnit]) = {
    nativeIdToUnit.values.exists(c.isInstance)
  }
  def geysirs = allByType[Geysir]
  def allCanDie = allByType[MaybeCanDie]
  def firstByType[T: Manifest]: Option[T] = {
    val lookFor = manifest[T].runtimeClass
    mine.find(lookFor.isInstance).map(_.asInstanceOf[T])
  }
  def mineByType[T: Manifest]: Iterator[T] = {
    val lookFor = manifest[T].runtimeClass
    mine.filter(lookFor.isInstance).map(_.asInstanceOf[T])
  }
  def mine = all.filter(_.nativeUnit.getPlayer == game.self())
  def minerals = allByType[MineralPatch]

  import scala.collection.JavaConverters._

  def allByType[T <: WrapsUnit : Manifest] = {
    val lookFor = manifest[T].runtimeClass.asInstanceOf[Class[T]]
    allByClass(lookFor)
  }
  def allByClass[T <: WrapsUnit](lookFor: Class[T]) = {
    def lazyCreate = {
      classIndexes += lookFor
      mutable.HashSet.empty ++= all.filter { e => lookFor.isInstance(e) }
    }
    val cached = preparedByClass.getOrElseUpdate(lookFor, lazyCreate)
    cached.asInstanceOf[collection.Set[T]]
  }
  def all = nativeIdToUnit.valuesIterator
  def tick(): Unit = {
    if (initial) {
      initial = false
      init()
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
        forces.get.isNotEnemy(u)
      } else {
        forces.get.isEnemy(u)
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
            info(s"Unit morphed from ${unit.initialNativeType} to ${u.getType}")
            if (unit.shouldReRegisterOnMorph) {
              val lifted = UnitWrapper.lift(u)
              registerUnit(u, lifted)
              fresh += lifted
              unit.onMorph(u.getType)
            }
          case _ => // noop
        }
      }
    }
  }
  private def ownAndNeutral = !hostile
  case class Forces(me: Player, allies: Set[Player]) {
    def isNotEnemy(u: bwapi.Unit) = !isEnemy(u)
    def isEnemy(u: bwapi.Unit) = !isNeutral(u) && !isFriend(u)
    def isFriend(u: bwapi.Unit) = {
      u.getPlayer == me || allies(u.getPlayer)
    }
    def isNeutral(u: bwapi.Unit) = u.getPlayer.getType == PlayerType.None
  }
}

class Grid2D(val cols: Int, val rows: Int, areaDataBitSet: scala.collection.BitSet,
             protected val containsBlocked: Boolean = true) extends Serializable {
  self =>
  private val lazyAreas = LazyVal.from {new AreaHelper(self).findFreeAreas}
  def areasIntersecting(area: Area):Set[Grid2D] = {
    import scala.collection.breakOut
    area.tiles.flatMap(areaWhichContainsAsFree)(breakOut)
  }
  def areaWhichContainsAsFree(tile: MapTilePosition) = areas
                                                       .find(e => e.inBounds(tile) && e.free(tile))
  def areas = lazyAreas.get
  def cuttingAreas(area:Area) = {
    var found = Option.empty[Grid2D]
    area.tiles.forall { where =>
      val check = areaWhichContainsAsFree(where)
      check.isEmpty || check == found
    }
  }
  def nearestFreeBlock(center: MapTilePosition, radius: Int) = {
    spiralAround(center).find { center =>
      containsAndFree(Area(center.movedBy(-radius, -radius), center.movedBy(radius, radius)))
    }
  }
  def containsAndFree(a: Area): Boolean = a.tiles.forall(containsAndFree)
  def containsAndFree(p: MapTilePosition): Boolean = inBounds(p) && free(p)
  def spiralAround(center: MapTilePosition, size: Int = 45) = new GeometryHelpers(cols, rows)
                                                              .iterateBlockSpiralClockWise(center,
                                                                size)
  def insideBounds(t: MapTilePosition) = {
    t.y >= 0 && t.x < cols && t.y >= 0 && t.y < rows
  }
  def areInSameWalkableArea(a: MapTilePosition, b: MapTilePosition) =
    areaWhichContainsAsFree(a).exists(_.free(b))
  def emptySameSize(blocked: Boolean) = new MutableGrid2D(cols, rows, mutable.BitSet.empty, blocked)
  def nearestFree(p: MapTilePosition) = {
    spiralAround(p).find(free)
  }
  def blockedMutableCopy = new MutableGrid2D(cols, rows, mutable.BitSet.empty, false)
  def asReadOnlyCopyIfMutable = this
  override def toString = s"$cols*$rows, $freeCount free"
  def freeCount = if (containsBlocked) size - areaDataBitSet.size else areaDataBitSet.size
  def size = cols * rows
  def outlineFree(a: Area): Boolean = a.outline.forall(free)
  def outlineFreeAndInBounds(a: Area): Boolean = inBounds(a) && a.outline.forall(free)
  def freeAndInBounds(a: Area): Boolean = a.tiles.forall(e => inBounds(e) && free(e))
  def inBounds(p: MapTilePosition): Boolean = inBounds(p.x, p.y)
  def free(p: MapTilePosition): Boolean = free(p.x, p.y)
  def anyBlocked(a: Area): Boolean = !inBounds(a) || !free(a)
  def free(a: Area): Boolean = a.tiles.forall(free)
  def inBounds(area: Area): Boolean = area.outline.forall(inBounds)
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
  def connectedByLine(a: MapTilePosition, b: MapTilePosition) = AreaHelper.directLineOfSight(a, b, this)
  def blockedCount = size - freeCount
  def mkString: String = mkString('x')
  def mkString(blockedDisplay: Char) = {
    0 until rows map { y =>
      0 until cols map { x =>
        if (free(x, y)) " " else blockedDisplay
      } mkString
    } mkString "\n"

  }
  def free(x: Int, y: Int): Boolean = {
    assert(inBounds(x, y), s"$x / $y is not inside $cols, $rows")
    val coord = x + y * cols
    if (containsBlocked) !areaDataBitSet(coord) else areaDataBitSet(coord)
  }
  def inBounds(x: Int, y: Int): Boolean = x >= 0 && x < cols && y >= 0 && y < rows
  def ensureContainsBlocked = if (containsBlocked)
    this
  else {
    val allBlockedIndexes = new mutable.BitSet ++= allBlocked.map(tileToIndex)
    new Grid2D(cols, rows, allBlockedIndexes.toImmutable)
  }
  def allBlocked = if (containsBlocked) bitSetToTiles else allIndexes.filterNot(areaDataBitSet).map(indexToTile)
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
  def mutableCopy = new
      MutableGrid2D(cols, rows, mutable.BitSet.fromBitMaskNoCopy(areaDataBitSet.toBitMask), containsBlocked)
  def allFree = if (containsBlocked) allIndexes.filterNot(areaDataBitSet).map(indexToTile) else bitSetToTiles
  private def allIndexes = Iterator.range(0, size)
  private def indexToTile(index: Int) = MapTilePosition.shared(index % cols, index / cols)
  private def bitSetToTiles = areaDataBitSet.iterator.map { index =>
    MapTilePosition.shared(index % cols, index / cols)
  }
  def free(position: MapTilePosition, area: Size): Boolean = {
    area.points.forall { p =>
      free(p.movedBy(position))
    }
  }
  def freeAndInBounds(position: MapTilePosition, size: Size): Boolean = {
    val area = Area(position, size)
    inBounds(area) && free(area)
  }
  def freeAndInBounds(position: MapTilePosition): Boolean = {
    inBounds(position) && free(position)
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
}

class MutableGrid2D(cols: Int, rows: Int, bitSet: mutable.BitSet, bitSetContainsBlocked: Boolean = true)
  extends Grid2D(cols, rows, bitSet, bitSetContainsBlocked) {
  def invertedMutable = {
    mutable.BitSet
  }

  def areaSize(anyContained: MapTilePosition) = {
    val isFree = free(anyContained)
    val on = if (isFree) this else reverseView
    AreaHelper.freeAreaSize(anyContained, on)
  }
  def reverseView: Grid2D = new Grid2D(cols, rows, bitSet, false)
  def anyFree = allFree.toStream.headOption

  override def areas = new AreaHelper(this).findFreeAreas

  def block_!(a: MapTilePosition, b: MapTilePosition): Unit = {
    AreaHelper.traverseTilesOfLine(a, b, block_!)
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

  override def asReadOnlyCopyIfMutable = new Grid2D(cols, rows, bitSet, containsBlocked)

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
  def block_!(tile: MapTilePosition): Unit = {
    block_!(tile.x, tile.y)
  }
  def free_!(tile: MapTilePosition): Unit = {
    free_!(tile.x, tile.y)
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
}

case class SerializablePatchGroup(areas: Seq[Area])

class ResourceAnalyzer(map: AnalyzedMap, myUnits: Units) {

  lazy val groups = myGroups.get.zipWithIndex.map { case (serializable, index) =>
    val pg = new MineralPatchGroup(index)
    serializable.areas.foreach { e =>
      val minerals = myUnits.minerals.find(_.area == e).getOr(s"Could not find minerals at $e")
      pg.addPatch(minerals)
    }
    pg
  }
  lazy val resourceAreas = {
    groups.map { patchGroup =>
      val geysirs = myUnits.geysirs
                    .filter(_.area.distanceTo(patchGroup.center) < 20)
                    .toSet

      ResourceArea(Some(patchGroup), geysirs)
    }
  }
  private val myGroups = FileStorageLazyVal.fromFunction({
    info(s"Calculating mineral groups...")
    val patchGroups = ArrayBuffer.empty[MineralPatchGroup]
    val allMins = ArrayBuffer.empty ++= myUnits.minerals
    val pathFinder = PathFinder.on(map.walkableGrid)
    allMins.foreach { mp =>
      patchGroups.find { g =>
        def isNew = !g.contains(mp)
        def isClose = {
          g.allTiles.exists { check =>
            val path = pathFinder.findPathNow(check, mp.tilePosition)
            path.isPerfectSolution && path.length < 20
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

    patchGroups.toSeq.map(e => SerializablePatchGroup(e.patches.map(_.area).toSeq))
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
  private val myPatches      = mutable.HashSet.empty[MineralPatch]
  private val myCenter       = new LazyVal[MapTilePosition](calcCenter)
  private val myValue        = new LazyVal[Int](myPatches.foldLeft(0)((acc, mp) => acc + mp.remaining))
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

  def remainingPercentage = myValue.get / myInitialValue.get.toDouble

  override def toString = s"Minerals#$patchId($value)@$center"
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
