package pony
package brain

import bwapi.Color
import pony.brain.modules.GatherMinerals

import scala.collection.mutable.ArrayBuffer

trait HasUniverse {
  def universe: Universe
  def units = universe.units
  def resources = universe.resources
  def world = universe.world
  def bases = universe.bases
}

abstract class AIModule[T <: WrapsUnit : Manifest](override val universe: Universe)
  extends Employer[T](universe) with HasUniverse {
  def ordersForTick: Traversable[UnitOrder]
  def onNth: Int = 1
}

class TwilightSparkle(world: DefaultWorld) {
  self =>

  private val universe: Universe = new Universe {
    override def bases: Bases = self.bases
    override def world: DefaultWorld = self.world
    override def resources: ResourceManager = self.resources
    override def units: UnitManager = unitManager
  }

  private val bases       = new Bases(world)
  private val resources   = new ResourceManager(universe)
  private val aiModules   = List(new GatherMinerals(universe))
  private val unitManager = new UnitManager(universe)

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

