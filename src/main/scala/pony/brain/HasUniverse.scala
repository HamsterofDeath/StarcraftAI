package pony
package brain

trait HasUniverse extends HasLazyVals {
  def race = forces.myself.scRace
  def plugins = universe.plugins
  def pluginByType[T: Manifest] = universe.pluginByType[T]
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
    ifNth(prime) {
      if (condition) {
        return body
      }
    }
    orElse
  }

  def ifNth(prime: PrimeNumber, firstTime: Option[PrimeNumber] = None)(u: => Unit) = {
    val execute = currentTick % prime.i == 0 ||
                  firstTime.exists(e => currentTick % e.i == 0)
    if (execute) {
      u
    }
  }

  protected implicit def implicitUniverse: Universe = universe

}
