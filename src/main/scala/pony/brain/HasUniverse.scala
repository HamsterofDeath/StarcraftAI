package pony
package brain

import scala.reflect.ClassTag

trait HasUniverse extends HasLazyVals {
  def race = forces.myself.scRace
  def plugins = universe.plugins
  def pluginByType[T: ClassTag] = universe.pluginByType[T]
  def pathfinders = universe.pathfinders
  def ferryManager = universe.ferryManager
  def unitGrid = universe.unitGrid
  def upgrades = universe.upgrades
  def time = universe.time
  def universe: Universe
  def unitManager = universe.unitManager
  def ownUnits = universe.ownUnits
  def enemies = universe.enemyUnits
  def resources = universe.resources
  def bases = universe.bases
  def currentTick = universe.currentTick
  def nativeGame = world.nativeGame
  def world = universe.world
  def strategicMap = universe.strategicMap
  def strategy = universe.strategy
  def worldDominationPlan = universe.worldDominationPlan
  def geoHelper = mapLayers.rawWalkableMap.geoHelper
  def mapLayers = universe.mapLayers
  def forces = universe.forces

  def mapNth[T](prime: PrimeNumber, orElse: T, condition: Boolean = true)(body: => T): T = {
    if (condition && isNth(prime)) body else orElse
  }

  def ifNth(prime: PrimeNumber, firstTime: Option[PrimeNumber] = None)(u: => Unit) = {
    if (isNth(prime, firstTime)) {
      u
    }
  }

  private def isNth(prime: PrimeNumber, firstTime: Option[PrimeNumber] = None) = {
    currentTick % prime.i == 0 || firstTime.exists(e => currentTick % e.i == 0)
  }

  protected implicit def implicitUniverse: Universe = universe

}
