package pony
package brain

import pony.brain.modules.Strategy.Strategies

trait Universe {
  def currentTick: Int
  def world: DefaultWorld
  def bases: Bases
  def resources: ResourceManager
  def unitManager: UnitManager
  def units: Units
  def mapLayers: MapLayers
  def strategicMap: StrategicMap
  def strategy: Strategies
  def race = bases.mainBase.mainBuilding.race
}