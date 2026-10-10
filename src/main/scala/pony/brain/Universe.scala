package pony
package brain

import pony.brain.budget.ResourceManager
import pony.brain.jobs.UnitManager
import pony.terrain.{MapLayers, StrategicMap}
import pony.units.{AllUnits, UnitGrid, Units, WrapsUnit}

import pony.brain.modules.strategy.StrategySelector
import pony.brain.modules.ferry.FerryManager
import pony.brain.modules.campaign.WorldDominationPlan

import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized
import scala.reflect.ClassTag

object Universe {
  var mainThread: Thread = uninitialized
}

trait Universe extends HasLazyVals {
  def forces: Forces

  Universe.mainThread = Thread.currentThread()

  def plugins: List[AIModule[? <: WrapsUnit]]

  def pluginByType[T: ClassTag] = {
    plugins.find(_.getClass == implicitly[ClassTag[T]].runtimeClass)
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

  def strategy: StrategySelector

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

  private def evalRace = (ownUnits.allMobiles.iterator ++ ownUnits.allBuildings.iterator).next()
    .mySCRace
}
