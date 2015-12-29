package pony
package brain

import java.text.DecimalFormat

import pony.brain.modules.Strategy.Strategies
import pony.brain.modules.{FerryManager, WorldDominationPlan}

import scala.collection.mutable.ArrayBuffer

trait Universe {
  private val myTime = new Time(this)
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
  def pathFinder: PathFinder
  def strategy: Strategies
  def unitGrid: UnitGrid
  def ferryManager: FerryManager
  def worldDominationPlan: WorldDominationPlan
  def resourceFields = world.resourceAnalyzer
  private def evalRace = (ownUnits.allMobiles.iterator ++ ownUnits.allBuildings.iterator).next.mySCRace
  private val race0 = LazyVal.from(evalRace)
  def myRace = race0.get
  def afterTick(): Unit = {
    afterTickListeners.foreach(_.postTick())
  }

  private val afterTickListeners = ArrayBuffer.empty[AfterTickListener]

  def register_!(listener: AfterTickListener): Unit = {
    afterTickListeners += listener
  }

  def unregister_!(listener: AfterTickListener): Unit = {
    afterTickListeners -= listener
  }

  def oncePerTick[T](gen: => T) = {
    val ret = LazyVal.from(gen)
    register_!(() => ret.invalidate())
    ret
  }
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
  def categoryName = {
    val phase = universe.strategy.current.timingHelpers.phase
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