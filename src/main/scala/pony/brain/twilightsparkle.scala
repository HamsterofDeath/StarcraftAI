package pony
package brain

import pony.brain.modules.Strategy.Strategies
import pony.brain.modules._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait HasUniverse extends HasLazyVals {
  def pathFinder = universe.pathFinder
  def ferryManager = universe.ferryManager
  def unitGrid = universe.unitGrid
  def upgrades = universe.upgrades
  def time = universe.time
  def race = universe.myRace
  def universe: Universe
  def unitManager = universe.unitManager
  def ownUnits = universe.ownUnits
  def enemies = universe.enemyUnits
  def resources = universe.resources
  def bases = universe.bases
  def currentTick = universe.currentTick
  def mapLayers = universe.mapLayers
  def nativeGame = world.nativeGame
  def world = universe.world
  def strategicMap = universe.strategicMap
  def strategy = universe.strategy
  def worldDominationPlan = universe.worldDominationPlan
  def ifNth(prime: PrimeNumber)(u: => Unit) = {
    if (universe.currentTick % prime.i == 0) {
      u
    }
  }
}

trait HasLazyVals {
  private val lazyVals  = ArrayBuffer.empty[LazyVal[_]]
  protected def currentTick: Int
  def oncePer[T <: AnyRef](prime: PrimeNumber, t: => T) = {
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

  def onTick(): Unit = {
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

  def result[T <: WrapsUnit](myJobs: Traversable[UnitWithJob[T]],
                             checkValidityNow: () => Boolean,
                             afterComputationDone: () => Unit): BackgroundComputationResult[T] = new
      BackgroundComputationResult[T] {

    override def jobs = myJobs
    override def repeatOrderIssue = checkValidityNow()
    override def afterComputation() = afterComputationDone()
    override def orders = jobs.flatMap(_.ordersForThisTick)
  }
}

trait ComputationIntensive[T <: WrapsUnit] extends AIModule[T] {
  type ComputationInput

  private var backgroundOp           = Future.successful(BackgroundComputationResult.nothing[T](() => {}))
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
          computationResult.jobs.foreach(assignJob_!)
          computationResult.orders
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
  extends AIModule[T](universe) with Orderless[T]

class HelperAIModule[T <: WrapsUnit : Manifest](universe: Universe) extends OrderlessAIModule[T](universe) {
  override def onTick(): Unit = {}
}

trait Orderless[T <: WrapsUnit] extends AIModule[T] {
  override def ordersForTick: Traversable[UnitOrder] = {
    onTick()
    Nil
  }

  def onTick(): Unit
}

object AIModule {
  def noop[T <: WrapsUnit : Manifest](universe: Universe) = new AIModule[T](universe) {
    override def ordersForTick: Traversable[UnitOrder] = Nil
  }
}

class TwilightSparkle(world: DefaultWorld) {
  self =>

  val universe: Universe = new Universe {
    override def bases = self.bases
    override def world = self.world
    override def upgrades = self.upgradeManager
    override def resources = self.resources
    override def unitManager = self.unitManager
    override def currentTick = world.tickCount
    override def mapLayers = self.maps
    override def ownUnits = world.ownUnits
    override def enemyUnits = world.enemyUnits
    override def strategicMap = world.strategicMap
    override def pathFinder = self.pathFinder
    override def strategy = self.strategy
    override def worldDominationPlan = self.worldDomination
    override def unitGrid = self.unitGrid
    override def ferryManager = self.ferryManager
  }
  private val bases           = new Bases(world)
  private val resources       = new ResourceManager(universe)
  private val strategy        = new Strategies(universe)
  private val worldDomination = new WorldDominationPlan(universe)
  private val aiModules      = List(
    new DefaultBehaviours(universe),
    new GatherMinerals(universe),
    new GatherGas(universe),
    new ProvideNewUnits(universe),
    new ProvideNewSupply(universe),
    new ProvideExpansions(universe),
    new ProvideNewBuildings(universe),
    new ProvideSuggestedAndRequestedAddons(universe),
    new HandleDefenses(universe),
    new EnqueueFactories(universe),
    new EnqueueArmy(universe),
    new ProvideUpgrades(universe),
    new JobReAssignments(universe),
    new SendOrdersToStarcraft(universe),
    AIModule.noop(universe)
  )
  private val unitManager    = new UnitManager(universe)
  private val upgradeManager = new UpgradeManager(universe)
  private val maps           = new MapLayers(universe)
  private val myPathFinder   = LazyVal.from {
    new PathFinder(maps)
  }
  private val unitGrid       = new UnitGrid(universe)
  private val ferryManager   = new FerryManager(universe)

  world.enemyUnits.registerKill_!(onKillOrCreate)
  world.ownUnits.registerKill_!(onKillOrCreate)
  world.enemyUnits.registerAdd_!(onKillOrCreate)
  world.ownUnits.registerAdd_!(onKillOrCreate)
  def plugins = aiModules
  def pluginByType[T <: AIModule[_]:Manifest] = {
    val c = manifest[T].runtimeClass
    aiModules.find(e => c.isAssignableFrom(e.getClass)).get.asInstanceOf[T]
  }
  def queueOrdersForTick(): Unit = {

    world.ownUnits.consumeFresh_! {_.init_!(universe)}
    world.enemyUnits.consumeFresh_! {_.init_!(universe)}

    maps.tick()
    resources.tick()
    unitManager.tick()
    strategy.tick()
    bases.tick()
    worldDomination.onTick()
    unitGrid.onTick()
    ferryManager.onTick()

    val tick = world.tickCount

    val activeInThisTick = aiModules.filter(e => tick == 0 || tick % e.onNth == 0)
    activeInThisTick.flatMap(_.ordersForTick).foreach(world.orderQueue.queue_!)

    universe.afterTick()

  }
  private def onKillOrCreate = (unit: WrapsUnit) =>
    unit match {
      case _: StaticallyPositioned =>
        myPathFinder.invalidate()
      case _ =>
    }
  private def pathFinder = myPathFinder.get
}

class Bases(world: DefaultWorld) {
  private val myBases          = ArrayBuffer.empty[Base]
  private val newBaseListeners = ArrayBuffer.empty[NewBaseListener]
  def isCovered(field: ResourceArea) = myBases.exists(_.resourceArea == field)
  def rich = {
    def singleValuable = myMineralFields.exists(_.value > 15000) && myMineralFields.exists(_.patches.size >= 10)
    def multipleIncomes = myMineralFields.size >= 2 && myMineralFields.map(_.value).sum > 5000
    def muchGas = myGeysirs.map(_.remaining).sum > 3000
    (singleValuable || multipleIncomes) && muchGas
  }
  def myMineralFields = myBases.flatMap(_.myMineralGroup).toSeq
  def myGeysirs = myBases.flatMap(_.myGeysirs).toSeq
  def mainBase = myBases.headOption
  def bases = myBases.toSeq
  def tick():Unit = {
    val all = world.ownUnits.allByType[MainBuilding]
    all.filterNot(known).foreach { main =>
      val newBase = new Base(main)(world)
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


case class Base(mainBuilding: MainBuilding)(world: DefaultWorld) {

  val resourceArea = world.resourceAnalyzer.resourceAreas.minBy(c => mainBuilding.area.distanceTo(c.center))

  val myMineralGroup = resourceArea.patches
  val myGeysirs      = resourceArea.geysirs

  info(
    s"""
       |Found base/minerals $mainBuilding: $myMineralGroup
     """.stripMargin)
  override def toString: String = s"Base@$mainBuilding"
}

