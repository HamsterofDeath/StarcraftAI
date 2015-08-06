package pony
package brain

import bwapi.Color
import pony.brain.modules.{GatherMinerals, ProvideNewUnits}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

trait HasUniverse {
  def universe: Universe
  def unitManager = universe.unitManager
  def units = universe.units
  def resources = universe.resources
  def world = universe.world
  def bases = universe.bases
  def currentTick = universe.currentTick
  def mapLayers = universe.mapsLayers
}

trait BackgroundComputationResult {
  def orders: Traversable[UnitOrder]
  def afterComputation: Unit
  def stillValid: Boolean
}

object BackgroundComputationResult {
  val nothing: BackgroundComputationResult = new BackgroundComputationResult {
    override def orders: Traversable[UnitOrder] = Nil
    override def stillValid: Boolean = false
    override def afterComputation: Unit = {}
  }

  def result(precalculatedOrders: Traversable[UnitOrder], checkValidityNow: () => Boolean,
             afterComputationDone: () => Unit): BackgroundComputationResult = new BackgroundComputationResult {
    override def stillValid = checkValidityNow()
    override def afterComputation = afterComputationDone()
    override def orders = precalculatedOrders
  }
}

trait ComputationIntensive[T <: WrapsUnit] extends AIModule[T] {
  type ComputationInput

  private var backgroundOp           = Future.successful(BackgroundComputationResult.nothing)
  private var currentResult          = Option.empty[BackgroundComputationResult]
  private var waitingForBackgroundOp = false

  override def ordersForTick: Traversable[UnitOrder] = {
    currentResult.filter(_.stillValid) match {
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
          info(s"Background computation finished, result is $computationResult")
          currentResult = Some(computationResult)
          waitingForBackgroundOp = false
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

  def evaluateNextOrders(in: ComputationInput): BackgroundComputationResult
}

abstract class AIModule[T <: WrapsUnit : Manifest](override val universe: Universe)
  extends Employer[T](universe) with HasUniverse {
  def ordersForTick: Traversable[UnitOrder]
  def onNth: Int = 1
}

object AIModule {
  def noop[T <: WrapsUnit : Manifest](universe: Universe) = new AIModule[T](universe) {
    override def ordersForTick: Traversable[UnitOrder] = Nil
  }
}

class TwilightSparkle(world: DefaultWorld) {
  self =>

  private val universe: Universe = new Universe {
    override def bases: Bases = self.bases
    override def world: DefaultWorld = self.world
    override def resources: ResourceManager = self.resources
    override def unitManager: UnitManager = self.unitManager
    override def currentTick = world.tickCount
    override def mapsLayers = self.maps
    override def units = world.units
  }

  private val bases     = new Bases(world)
  private val resources = new ResourceManager(universe)

  private val aiModules   = List(
    new GatherMinerals(universe),
    new ProvideNewUnits(universe),
    AIModule.noop(universe)
  )
  private val unitManager = new UnitManager(universe)
  private val maps        = new MapLayers(universe)

  def queueOrdersForTick(): Unit = {
    if (world.isFirstTick) {
      bases.findMainBase()
    }

    bases.tick()
    resources.tick()
    unitManager.tick()

    val tick = world.tickCount

    val activeInThisTick = aiModules.filter(e => tick == 0 || tick % e.onNth == 0)
    activeInThisTick.flatMap(_.ordersForTick).foreach(world.orderQueue.queue_!)
  }
}

class Bases(world: DefaultWorld) {
  private val myBases = ArrayBuffer.empty[Base]

  def mainBase = myBases.head

  def tick(): Unit = {
    myBases.foreach(_.tick())
  }
  def bases = myBases.toSeq

  def findMainBase(): Unit = {
    world.units.firstByType[MainBuilding].foreach {myBases += new Base(world, _)}
  }
}

case class Base(world: DefaultWorld, mainBuilding: MainBuilding) {
  val myMineralGroup = world.mineralPatches.nearestTo(mainBuilding.tilePosition)
  def tick(): Unit = {
    world.debugger.debugRender { renderer =>
      world.mineralPatches.groups.foreach { mpg =>
        mpg.patches.foreach { mp =>
          renderer.in_!(Color.Green).writeText(mp.tilePosition, mpg.patchId)
        }
      }
    }
  }

  info(
    s"""
       |Found base/minerals $mainBuilding: $myMineralGroup
     """.stripMargin)
  override def toString: String = s"Base@$mainBuilding"
}

