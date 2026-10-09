package pony

import bwapi.Game
import pony.brain.{Supplies, Universe}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

class DefaultWorld(game: Game) extends WorldListener with WorldEventDispatcher {
  private var myUniverse: Universe = uninitialized

  def init_!(universe: Universe) = {
    this.myUniverse = universe
  }

  def universe = myUniverse

  def addPostTickAction(value: => Unit) = {
    postTickActions += (() => value)
  }

  // these must be initialized after the first tick. making them lazy solves this
  lazy val resourceAnalyzer = new ResourceAnalyzer(
    map,
    AllUnits(myUniverse.ownUnits, myUniverse.enemyUnits)
  )
  lazy val strategicMap = new StrategicMap(resourceAnalyzer.resourceAreas, map.walkableGrid, game)

  val map                      = new AnalyzedMap(game)
  val debugger                 = new Debugger(game, this)
  val orderQueue               = new OrderQueue(game, debugger)
  private val removeQueueOwn   = ArrayBuffer.empty[bwapi.Unit]
  private val removeQueueEnemy = ArrayBuffer.empty[bwapi.Unit]
  private val destroyedEnemies = mutable.Set.empty[Int]
  def observedDestroyedEnemies = destroyedEnemies.toSet
  private var ticks            = 0
  private val postTickActions  = ArrayBuffer.empty[() => Unit]

  def nativeGame = game

  def currentResources = {
    val self  = game.self()
    val total = self.supplyTotal()
    val used  = self.supplyUsed()
    Resources(self.minerals(), self.gas(), Supplies(used, total))
  }

  def isFirstTick = ticks == 0

  def tickCount = ticks

  override def onUnitDestroy(unit: bwapi.Unit): Unit = {
    super.onUnitDestroy(unit)
    if (unit.getPlayer.isEnemy(game.self())) {
      destroyedEnemies += unit.getID
      removeQueueEnemy += unit
    } else {
      removeQueueOwn += unit
    }
  }

  def tick(): Unit = {
    myUniverse.ownUnits.dead_!(removeQueueOwn.toSeq)
    myUniverse.enemyUnits.dead_!(removeQueueEnemy.toSeq)
    removeQueueOwn.clear()
    removeQueueEnemy.clear()
    myUniverse.ownUnits.tick()
    myUniverse.enemyUnits.tick()
  }

  def postTick(): Unit = {
    debugger.renderer.allow()
    postTickActions.foreach(e => e())
    debugger.renderer.disallow()
    postTickActions.clear()
    orderQueue.debugAll()
    orderQueue.issueAll(1)
    ticks += 1
  }
}

object DefaultWorld {
  def spawn(game: Game) = new DefaultWorld(game)
}
