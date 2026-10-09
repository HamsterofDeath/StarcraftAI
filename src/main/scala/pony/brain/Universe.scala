package pony
package brain

import pony.brain.modules.Strategy.Strategies
import pony.brain.modules.{FerryManager, WorldDominationPlan}

import scala.collection.mutable.ArrayBuffer

object Universe {
  var mainThread: Thread = _
}

trait Universe extends HasLazyVals {
  def forces: Forces

  Universe.mainThread = Thread.currentThread()

  def plugins: List[AIModule[_ <: WrapsUnit]]

  def pluginByType[T: Manifest] = {
    plugins.find(_.getClass == implicitly[Manifest[T]].runtimeClass)
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
