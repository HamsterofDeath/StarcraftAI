package pony
package brain

import java.text.DecimalFormat

import pony.brain.modules.Strategy.Strategies

trait Universe {
  val time = new Time(this)
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

class Time(universe: Universe) {
  private val format = new DecimalFormat("00")
  def formatted = s"${(format.format(hours))}:${format.format(minutes)}:${format.format(seconds)}"
  def hours = minutes / 60.0
  def minutes = seconds / 60.0
  def seconds = universe.currentTick / 24.0
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