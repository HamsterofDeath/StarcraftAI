package pony
package brain

import pony.brain.modules.Strategy.Strategies
import pony.brain.modules._

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.reflect.ManifestFactory

trait HasUniverse extends HasLazyVals {
  def race = forces.myself.scRace
  def plugins = universe.plugins
  def pluginByType[T: Manifest] = universe.pluginByType[T]
  def pathfinders = universe.pathfinders
  def ferryManager = universe.ferryManager
  def unitGrid = universe.unitGrid
  def upgrades = universe.upgrades
  def time = universe.time
  def universe: Universe
  def unitManager = universe.unitManager
  def ownUnits = universe.ownUnits
  def enemies = universe.enemyUnits
  def resources = universe.resources
  def bases = universe.bases
  def currentTick = universe.currentTick
  def nativeGame = world.nativeGame
  def world = universe.world
  def strategicMap = universe.strategicMap
  def strategy = universe.strategy
  def worldDominationPlan = universe.worldDominationPlan
  def geoHelper = mapLayers.rawWalkableMap.geoHelper
  def mapLayers = universe.mapLayers
  def forces = universe.forces

  def mapNth[T](prime: PrimeNumber, orElse: T, condition: Boolean = true)(body: => T): T = {
    ifNth(prime) {
      if (condition) {
        return body
      }
    }
    orElse
  }

  def ifNth(prime: PrimeNumber, firstTime: Option[PrimeNumber] = None)(u: => Unit) = {
    val execute = currentTick % prime.i == 0 ||
                  firstTime.exists(e => currentTick % e.i == 0)
    if (execute) {
      u
    }
  }

  protected implicit def implicitUniverse: Universe = universe

}

trait IsTicked {

  private var lastCalled = 0

  def onTick_!(): Unit = {
    lastCalled = currentTick
  }

  def assertCalled() = {
    if (lastCalled != currentTick) {
      onTick_!()
    }
  }

  protected def currentTick: Int

}

trait HasLazyVals extends IsTicked {
  private var counter = 0

  def uniqueKey = {
    val value = counter
    counter += 1
    Key(value)
  }

  case class Key(private val id: Int)

  private      val lazyVals              = ArrayBuffer.empty[LazyVal[_]]
  private lazy val lockedGroupedLazyVals = mutable.HashMap.empty[Key, ArrayBuffer[LazyVal[_]]]

  def explicitly[T](key: Key, t: => T, synchronize: Boolean = false) = {
    val newLazyVal = {
      if (synchronize) {
        SynchronizedLazyVal.from(t)
      } else {
        LazyVal.from(t)
      }
    }
    lockedGroupedLazyVals.getOrElseUpdate(key, ArrayBuffer.empty) += newLazyVal
    newLazyVal
  }

  def invalidate(key: Key) = {
    lockedGroupedLazyVals.get(key).foreach(_.foreach(_.invalidate()))
  }

  def oncePer[T](prime: PrimeNumber)(t: => T) = {
    var store: T = null.asInstanceOf[T]
    oncePerTick {
      if (store == null || currentTick % prime.i == 0) {
        store = t
      }
      store
    }
  }

  def oncePerTick[T](t: => T) = {
    val l = LazyVal.from(t)
    lazyVals += l
    l
  }

  def once[T](t: => T) = {
    SynchronizedLazyVal.from(t)
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    lazyVals.foreach(_.invalidate())
  }
}

trait BackgroundComputationResult[T <: WrapsUnit] {

  def jobs: Traversable[UnitWithJob[T]]

  def orders = jobs.flatMap(_.ordersForThisTick)
  def afterComputation(): Unit
  def repeatOrderIssue: Boolean
}

object BackgroundComputationResult {
  def nothing[T <: WrapsUnit](cleanUp: () => Unit): BackgroundComputationResult[T] = new
      BackgroundComputationResult[T] {
    override def repeatOrderIssue: Boolean = false

    override def afterComputation(): Unit = cleanUp()

    override def jobs: Traversable[UnitWithJob[T]] = Nil
  }

  def result[T <: WrapsUnit, J <: UnitWithJob[T]](myJobs: Traversable[() => J],
                                                  checkValidityNow: () => Boolean,
                                                  canCreateNow: () => Boolean = () => true)
                                                 (afterComputationDone: Traversable[J] => Unit) = new
      BackgroundComputationResult[T] {

    private lazy val executed = if (canCreateNow()) myJobs.map(_.apply()) else Nil

    override def jobs = executed

    override def repeatOrderIssue = checkValidityNow()

    override def afterComputation() = afterComputationDone(executed)

    override def orders = jobs.flatMap(_.ordersForThisTick)
  }
}

trait BackgroundComputation[T <: WrapsUnit] extends AIModule[T] {
  type ComputationInput

  private var backgroundOp           = Future
                                       .successful(BackgroundComputationResult.nothing[T](() => {}))
  private var currentResult          = Option.empty[BackgroundComputationResult[T]]
  private var waitingForBackgroundOp = false

  override def ordersForTick: Traversable[UnitOrder] = {
    currentResult.filter(_.repeatOrderIssue) match {
      // reuse current computation result as long as it is valid
      case Some(result) => result.orders
      case None =>
        // kick old result
        currentResult = None
        if (waitingForBackgroundOp && !backgroundOp.isCompleted) {
          // waiting, but no result yet: noop
          Nil
        } else if (waitingForBackgroundOp && backgroundOp.isCompleted) {
          // we were waiting for a computation to finish
          val computationResult = Await.result(backgroundOp, Duration.Zero)
          computationResult.afterComputation()
          info(s"Background computation finished, result is $computationResult")
          currentResult = Some(computationResult)
          waitingForBackgroundOp = false
          computationResult.jobs.foreach {
            case switch: CanAcceptUnitSwitch[T] =>
              // time has passed, pick a new unit for this job if possible
              val req = switch.asRequest
              implicit val summonedManifest: Manifest[T] =
                ManifestFactory.classType(switch.unit.getClass)
              val candidates = {
                def recycle = {
                  Set(switch: UnitWithJob[T])
                  .filter(job => unitManager.jobOptOf(switch.unit).contains(job))
                }
                unitManager.requestWithoutTracking[T](req, recycle)
              }
              var assigned = false
              candidates.headOption.foreach { replacement =>
                if (switch.failedOrObsolete) {
                  warn(s"Construction job failed while calculations were ongoing")
                } else {
                  if (replacement != switch.unit) {
                    val newRequest = switch.copyOfJobForNewUnit(replacement)
                    trace(s"Unit ${switch.unit} replaced by $replacement")
                    assignJob_!(newRequest)
                    assigned = true
                  } else {
                    trace(s"Unit ${switch.unit} kept its job after a background calculation",
                      replacement == switch.unit)
                    assignJob_!(switch)
                    assigned = true
                  }
                }
              }
              warn(
                s"Background calculation finished, but no unit could do the job anymore: $switch",
                candidates.isEmpty)
              if (!assigned) {
                switch.fail_!()
                // Unassigned jobs will never reach UnitManager's normal terminal listeners.
                if (!unitManager.jobOptOf(switch.unit).contains(switch)) switch.onFinishOrFail()
              }
            case job =>
              trace(s"Unit ${job.unit} kept its job after a background calculation")
              assignJob_!(job)
          }
          Nil
        } else {
          // we are not waiting
          calculationInput match {
            case None => Nil
            case Some(in) =>
              info(s"Background computation starting, input is $in")
              backgroundOp = Future {evaluateNextOrders(in)}
              waitingForBackgroundOp = true
              Nil
          }
        }
    }
  }

  def calculationInput: Option[ComputationInput]

  /**
    * do not access the universe here, it's running in another thread!
    */
  def evaluateNextOrders(in: ComputationInput): BackgroundComputationResult[T]
}

abstract class AIModule[T <: WrapsUnit : Manifest](override val universe: Universe)
  extends Employer[T](universe) with HasUniverse {
  def ordersForTick: Traversable[UnitOrder]

  def onNth: Int = 1

  override def toString = s"Module ${getClass.className}"

  def renderDebug(renderer: Renderer): Unit = {}
}

abstract class OrderlessAIModule[T <: WrapsUnit : Manifest](universe: Universe)
  extends AIModule[T](universe) with Orderless[T] {
  def debugText = ""
}

class HelperAIModule[T <: WrapsUnit : Manifest](universe: Universe)
  extends OrderlessAIModule[T](universe) {
  override def onTick_!(): Unit = {}
}

trait Orderless[T <: WrapsUnit] extends AIModule[T] {
  override def ordersForTick: Traversable[UnitOrder] = {
    onTick_!()
    Nil
  }

  def onTick_!(): Unit
}

object AIModule {
  def noop[T <: WrapsUnit : Manifest](universe: Universe) = new AIModule[T](universe) {
    override def ordersForTick: Traversable[UnitOrder] = Nil
  }
}

class TwilightSparkle(world: DefaultWorld) {
  self =>
  def renderDebug(renderer: Renderer) = {
    worldDomination.renderDebug(renderer)
  }


  val universe: Universe = new Universe {

    override def pathfinders = new Pathfinders {
      override def groundSafe = myPathFinderGroundSafe

      override def ground = myPathFinderGround

      override def airSafe = myPathFinderAirSafe
    }

    override def bases = self.bases

    override def world = self.world

    override def upgrades = self.upgradeManager

    override def resources = self.resources

    override def unitManager = self.unitManager

    override def currentTick = world.tickCount

    override def mapLayers = self.maps

    override def ownUnits = self.ownUnits

    override def enemyUnits = self.enemyUnits

    override def strategicMap = world.strategicMap

    override def strategy = self.strategy

    override def worldDominationPlan = self.worldDomination

    override def unitGrid = self.unitGrid

    override def ferryManager = self.ferryManager

    override def plugins = aiModules

    override def forces = self.forces
  }
  world.init_!(universe)
  private val ownUnits        = new Units(world.nativeGame, false, universe)
  private val enemyUnits      = new Units(world.nativeGame, true, universe)
  private val bases           = new Bases(world, universe)
  private val resources       = new ResourceManager(universe)
  private val strategy        = new Strategies(universe)
  private val worldDomination = new WorldDominationPlan(universe)
  private val sendOrders      = new SendOrdersToStarcraft(universe)
  // Failed hiring requests live two frames; satisfying them must not wait for a heavy tick.
  private val provideNewUnits = new ProvideNewUnits(universe)
  private val aiModules       = List(
    new DefaultBehaviours(universe),
    new ManageMiningAtBases(universe),
    new ManageMiningAtGeysirs(universe),
    new ProvideSpareSCVs(universe),
    new ProvideNewSupply(universe),
    new ProvideNewBuildings(universe),
    new ProvideSuggestedAndRequestedAddons(universe),
    new HandleDefenses(universe),
    new TerranBunkerDefense(universe),
    new RunTerranCampaign(universe),
    new SetupAntiCloakDefenses(universe),
    new EnqueueFactories(universe),
    new EnqueueArmy(universe),
    new ProvideUpgrades(universe),
    new ProvideExpansions(universe),
    new JobReAssignments(universe),
    AIModule.noop[WrapsUnit](universe)
  )

  private val forces = {
    val game = world.nativeGame
    val me = game.self
    val friends = game.allies()
    val enemies = game.enemies()
    new Forces(me, friends.asScala.toSet, enemies.asScala.toSet, universe)
  }

  private val unitManager            = new UnitManager(universe)
  private val upgradeManager         = new UpgradeManager(universe)
  private val maps                   = new MapLayers(universe)
  private val myPathFinderGround     = LazyVal.from {
    new PathFinder(maps, false, true)
  }
  private val myPathFinderAirSafe    = universe.oncePerTick {
    new PathFinder(maps, true, false)
  }
  private val myPathFinderGroundSafe = universe.oncePerTick {
    new PathFinder(maps, true, true)
  }
  private val unitGrid               = new UnitGrid(universe)
  private val ferryManager           = new FerryManager(universe)

  enemyUnits.registerKill_!(onKillOrCreate)
  ownUnits.registerKill_!(onKillOrCreate)
  enemyUnits.registerAdd_!(onKillOrCreate)
  ownUnits.registerAdd_!(onKillOrCreate)

  def plugins = aiModules

  def pluginByType[T <: AIModule[_] : Manifest] = {
    val c = implicitly[Manifest[T]].runtimeClass
    aiModules.find(e => c >= e.getClass).get.asInstanceOf[T]
  }

  def queueOrdersForTick(): Unit = {

    ownUnits.consumeFresh_! {_.init_!(universe)}
    enemyUnits.consumeFresh_! {_.init_!(universe)}

    universe.onTick_!()

    // Cheap per-frame pipeline: job lifecycle, command emission and action bookkeeping must
    // advance every frame so interruptability and order-lock semantics keep their frame meaning.
    unitManager.tick()

    // Satisfy failed hiring requests while they are still alive (they clear after two frames).
    provideNewUnits.onTick_!()

    val tick = world.tickCount
    if (AiCadence.heavyNow(tick)) {
      // Expensive planning runs once per heavy tick (a game second by default).
      maps.tick()
      resources.tick()
      strategy.tick()
      bases.tick()
      worldDomination.onTick_!()
      unitGrid.onTick_!()
      ferryManager.onTick_!()

      val heavyTick = tick / AiCadence.frames
      val activeInThisTick = aiModules.filter(e => tick == 0 || heavyTick % math.max(1, e.onNth / AiCadence.frames) == 0)
      activeInThisTick.flatMap(_.ordersForTick).foreach(world.orderQueue.queue_!)
    }

    // Emit job orders after this frame's planning so freshly hired jobs command their units
    // in the same frame (keeps the order-history liveness check from failing new jobs).
    sendOrders.ordersForTick.foreach(world.orderQueue.queue_!)
    universe.afterTick()

  }

  private def onKillOrCreate = (unit: WrapsUnit) =>
    unit match {
      case _: StaticallyPositioned =>
        myPathFinderGroundSafe.invalidate()
        myPathFinderGround.invalidate()
        myPathFinderAirSafe.invalidate()
      case _ =>
    }
}

class Bases(world: DefaultWorld, override val universe: Universe) extends HasUniverse {
  private val myBases          = ArrayBuffer.empty[Base]
  private val newBaseListeners = ArrayBuffer.empty[NewBaseListener]

  def isCovered(field: ResourceArea) = myBases.exists(b =>
    !b.mainBuilding.isFloating && b.mainBuilding.isInGame && b.resourceArea.contains(field))

  def rich = {
    def singleValuable = myMineralFields.exists(_.value > 15000) &&
                         myMineralFields.exists(_.patches.size >= 10)
    def multipleIncomes = myMineralFields.size >= 2 && myMineralFields.map(_.value).sum > 5000
    def muchGas = myGeysirs.map(_.remaining).sum > 3000
    (singleValuable || multipleIncomes) && muchGas
  }

  def myMineralFields = myBases.flatMap(_.myMineralGroup).immutableView

  def myGeysirs = myBases.flatMap(_.myGeysirs).immutableView

  def richBasesCount = richBases.size

  def richBases = allBases.filter(_.resourceArea.exists(_.rich))

  def allBases = myBases.immutableView

  def finishedBases = allBases.filterNot(_.mainBuilding.isBeingCreated)

  def mainBase = myBases.headOption

  def tick(): Unit = {
    val all = ownUnits.allByType[MainBuilding]
    all.filterNot(known).foreach { main =>
      val newBase = new Base(main)
      myBases += newBase
      newBaseListeners.foreach(_.newBase(newBase))
    }

    myBases.retain(!_.mainBuilding.isDead)
  }

  def known(mb: MainBuilding) = myBases.exists(_.mainBuilding == mb)

  def register(newBaseListener: NewBaseListener, notifyForExisting: Boolean): Unit = {
    newBaseListeners += newBaseListener
    if (notifyForExisting) {
      myBases.foreach(newBaseListener.newBase)
    }
  }
}

trait NewBaseListener {
  def newBase(base: Base): Unit
}

case class Base(mainBuilding: MainBuilding) {

  def world = mainBuilding.world

  def resourceArea = {
    world.resourceAnalyzer.resourceAreas
    .minByOpt { c =>
      mainBuilding.area.distanceTo(c.center)
    }
  }

  // Bind resource geometry on the native callback thread before the alternative-path future starts.
  // The current field remains dynamic so a lifted depot can later rebind after landing.
  private val initialResourceArea = resourceArea

  def alternativeResourceAreas = myAlternativeResourceAreas.result

  private val myAlternativeResourceAreas = {
    val safeGround = mainBuilding.pathfinders.groundSafe
    val safeAir = mainBuilding.pathfinders.airSafe
    val tile = mainBuilding.centerTile

    def sortByPath = {

      def evaluate(area: ResourceArea, ground: Boolean) = {
        val finder = if (ground) safeGround else safeAir
        val path = safeGround.findSimplePathNow(tile, area.anyTile)

        path match {
          case None =>
            None
          case Some(p) =>
            Some(area -> p.length)

        }
      }
      val ground = world.resourceAnalyzer.resourceAreas.flatMap(evaluate(_, true))

      val air = world.resourceAnalyzer.resourceAreas.filterNot { area =>
        ground.exists(_._1 == area)
      }.flatMap(evaluate(_, false))

      val all = ground.sortBy(_._2).map(_._1) ++ air.sortBy(_._2).map(_._1)
      all.filterNot(initialResourceArea.contains).filter { candidate =>
        mainBuilding.mapLayers.rawWalkableMap
        .areInSameWalkableArea(candidate.anyTile, tile)
      }
    }

    BWFuture(sortByPath, Nil)
  }

  def myMineralGroup = resourceArea.flatMap(_.patches)
  def myGeysirs      = resourceArea.map(_.geysirs).getOrElse(Set.empty)

  info(
    s"""
       |Found base/minerals $mainBuilding: $myMineralGroup
     """.stripMargin)

  override def toString: String = s"Base@$mainBuilding"
}

