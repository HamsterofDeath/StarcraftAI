package pony
package brain

import pony.brain.modules.Strategy.Strategies
import pony.brain.modules._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait HasUniverse {
  def time = universe.time
  def race = universe.race
  def universe: Universe
  def unitManager = universe.unitManager
  def units = universe.units
  def resources = universe.resources
  def bases = universe.bases
  def currentTick = universe.currentTick
  def mapLayers = universe.mapLayers
  def nativeGame = world.nativeGame
  def world = universe.world
  def strategicMap = universe.strategicMap
  def strategy = universe.strategy
  def ifNth(nth: Int)(u: => Unit) = {
    if (universe.currentTick % nth == 0) {
      u
    }
  }
}

trait BackgroundComputationResult[T <: WrapsUnit] {

  def jobs: Traversable[UnitWithJob[T]]

  def orders = jobs.flatMap(_.ordersForThisTick)
  def afterComputation(): Unit
  def repeatOrderIssue: Boolean
}

object BackgroundComputationResult {
  def nothing[T <: WrapsUnit]: BackgroundComputationResult[T] = new BackgroundComputationResult[T] {
    override def repeatOrderIssue: Boolean = false
    override def afterComputation(): Unit = {}
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

  private var backgroundOp           = Future.successful(BackgroundComputationResult.nothing[T])
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
    override def bases: Bases = self.bases
    override def world: DefaultWorld = self.world
    override def resources: ResourceManager = self.resources
    override def unitManager: UnitManager = self.unitManager
    override def currentTick = world.tickCount
    override def mapLayers = self.maps
    override def units = world.units
    override def strategicMap = world.strategicMap
    override def strategy = self.strategy
  }

  private val bases     = new Bases(world)
  private val resources = new ResourceManager(universe)
  private val strategy  = new Strategies(universe)

  private val aiModules   = List(
    new GatherMinerals(universe),
    new GatherGas(universe),
    new ProvideNewUnits(universe),
    new ProvideNewSupply(universe),
    new ProvideNewBuildings(universe),
    new ProvideAddons(universe),
    new ProvideFactories(universe),
    new ProvideArmy(universe),
    new ProvideUpgrades(universe),
    new JobReAssignments(universe),
    new OrderBridge(universe),
    AIModule.noop(universe)
  )
  private val unitManager = new UnitManager(universe)
  private val maps        = new MapLayers(universe)

  def queueOrdersForTick(): Unit = {
    if (world.isFirstTick) {
      bases.findMainBase()
    }

    maps.tick()
    resources.tick()
    unitManager.tick()
    strategy.tick()

    val tick = world.tickCount

    val activeInThisTick = aiModules.filter(e => tick == 0 || tick % e.onNth == 0)
    activeInThisTick.flatMap(_.ordersForTick).foreach(world.orderQueue.queue_!)
  }
}

class Bases(world: DefaultWorld) {
  private val myBases = ArrayBuffer.empty[Base]
  def rich = {
    val singleValuable = myMineralFields.exists(_.value > 15000)
    val multipleIncomes = myMineralFields.size >= 2 && myMineralFields.map(_.value).sum > 2000
    val muchGas = myGeysirs.map(_.remaining).sum > 2000
    (singleValuable || multipleIncomes) && muchGas
  }
  def myMineralFields = myBases.flatMap(_.myMineralGroup).toSeq
  def myGeysirs = myBases.flatMap(_.myGeysirs).toSeq
  def mainBase = myBases.head

  def bases = myBases.toSeq

  def findMainBase(): Unit = {
    world.units.firstByType[MainBuilding].foreach {myBases += new Base(_)(world)}
  }
}

case class Base(mainBuilding: MainBuilding)(world: DefaultWorld) {

  val myMineralGroup = world.mineralPatches.nearestTo(mainBuilding.tilePosition)
  val myGeysirs      = world.units.geysirs
                       .filter(g => mainBuilding.area.distanceTo(g.area) < 5)
                       .toSet

  info(
    s"""
       |Found base/minerals $mainBuilding: $myMineralGroup
     """.stripMargin)
  override def toString: String = s"Base@$mainBuilding"
}

