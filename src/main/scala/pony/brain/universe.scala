package pony
package brain

import java.text.DecimalFormat

import pony.brain.modules.Strategy.Strategies
import pony.brain.modules.{FerryManager, WorldDominationPlan}

import scala.collection.mutable.ArrayBuffer

trait Pathfinders {
  def ground: PathFinder
  def groundSafe: PathFinder
  def airSafe: PathFinder
  def safeFor[T <: Mobile](m: T) = {
    m match {
      case g: GroundUnit if g.onGround => groundSafe
      case a: AirUnit => airSafe
      case _ => !!!(s"Invalid request: $m")
    }
  }
}

object Universe {
  var mainThread: Thread = _
}

trait Universe extends HasLazyVals {
  def forces: Forces

  Universe.mainThread = Thread.currentThread()

  def plugins: List[AIModule[_ <: WrapsUnit]]

  def pluginByType[T: Manifest] = {
    plugins.find(_.getClass == manifest[T].runtimeClass)
    .get
    .asInstanceOf[T]
  }

  private val myTime             = new Time(this)
  private val afterTickListeners = ArrayBuffer.empty[AfterTickListener]

  def pathfinders: Pathfinders

  def allUnits = AllUnits(ownUnits, enemyUnits)

  def time = myTime

  def currentTick: Int

  def world: DefaultWorld

  def upgrades: UpgradeManager

  def bases: Bases

  def resources: ResourceManager

  def unitManager: UnitManager

  def ownUnits: Units

  def enemyUnits: Units

  def mapLayers: MapLayers

  def strategicMap: StrategicMap

  def strategy: Strategies

  def unitGrid: UnitGrid

  def ferryManager: FerryManager

  def worldDominationPlan: WorldDominationPlan

  def resourceFields = world.resourceAnalyzer

  def afterTick(): Unit = {
    afterTickListeners.foreach(_.postTick())
  }

  def register_!(listener: AfterTickListener): Unit = {
    afterTickListeners += listener
  }

  def unregister_!(listener: AfterTickListener): Unit = {
    afterTickListeners -= listener
  }

  private def evalRace = (ownUnits.allMobiles.iterator ++ ownUnits.allBuildings.iterator).next
                         .mySCRace
}

trait AfterTickListener {

  def postTick(): Unit
}

class Time(universe: Universe) {
  private val format = new DecimalFormat("00")

  def formatted = {
    val sec = seconds.toInt
    val min = sec / 60
    val hour = min / 60
    s"${format.format(hour)}:${format.format(min % 60)}:${format.format(sec % 60)}"
  }

  def seconds = universe.currentTick / 24.0

  def hours = minutes / 60.0

  def minutes = seconds / 60.0

  def phase = universe.strategy.current.timingHelpers.phase

  def categoryName = {
    if (phase.isEarly) {
      "Early game"
    } else if (phase.isEarlyMid) {
      "Early mid game"
    } else if (phase.isMid) {
      "Mid game"
    } else if (phase.isLateMid) {
      "Late Mid game"
    } else if (phase.isLate) {
      "Late game"
    } else
      throw new IllegalStateException(s"Time started to work in unexpected ways")
  }
}