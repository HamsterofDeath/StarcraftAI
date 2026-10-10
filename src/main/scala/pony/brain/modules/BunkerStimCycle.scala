package pony
package brain
package modules

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * Marines in a bunker fire faster stimmed. When enemies close in on a bunker, its Marines whose stim ran out step
  * out, stim and go straight back in (EnterDefensiveBunker stims them on the way): before the enemy is in reach all
  * of them at once, once the bunker fights one at a time, so it never stops firing.
  */
class BunkerStimCycle(universe: Universe) extends OrderlessAIModule[Bunker](universe) {
  import BunkerStim._

  private val lastUnload = mutable.HashMap.empty[Int, Int]
  private var threatenedIds = Set.empty[Int]

  /** Whether enemies close in on this bunker while stim is researched. */
  def threatened(bunkerId: Int) = threatenedIds(bunkerId)

  private def enemyNear(b: Bunker, tiles: Int) =
    unitGrid.enemy.allInRange[Mobile](b.centerTile, tiles).exists(e => !e.isHarmlessNow && !e.isInstanceOf[WorkerUnit])

  override def onTick_!(): Unit = {
    if (!nativeGame.self().hasResearched(bwapi.TechType.Stim_Packs)) {
      threatenedIds = Set.empty
      return
    }
    val bunkers = ownUnits.allByType[Bunker].filter(b => b.isInGame && !b.isBeingCreated).toVector
    threatenedIds = bunkers.filter(enemyNear(_, ThreatTiles)).map(_.nativeUnitId).toSet
    val defense = universe.pluginByType[TerranBunkerDefense]
    bunkers.filter(b => threatenedIds(b.nativeUnitId)).foreach { b =>
      val id = b.nativeUnitId
      if (currentTick - lastUnload.getOrElse(id, -UnloadEvery) >= UnloadEvery) {
        val native  = b.nativeUnit
        val loaded  = native.getLoadedUnits.asScala.toVector.filter(_.getType == bwapi.UnitType.Terran_Marine)
        val inside  = loaded.map(u => Inside(u.getID, u.getHitPoints, u.getStimTimer))
        val outside = ownUnits.allByType[Marine].exists(m =>
          m.isInGame && !m.nativeUnit.isLoaded && defense.bunkerFor(m).exists(_.nativeUnitId == id)
        )
        val unload = toUnload(inside, enemyNear(b, ReachTiles), outside)
        if (unload.nonEmpty) {
          lastUnload(id) = currentTick
          loaded.filter(u => unload.contains(u.getID)).foreach(native.unload)
          NativeMatchEvidence.trace("bunker-stim", s"bunker=$id marines=${unload.mkString(",")} inside=${inside.size}")
        }
      }
    }
  }
}

/** Which Marines leave a bunker to stim, kept free of the game. */
private[pony] object BunkerStim {

  /** A Marine inside: its hit points and how long its stim lasts still (0: run out). */
  final case class Inside(id: Int, hitPoints: Int, stimTimer: Int)

  /** Stim costs ten hit points; below this a Marine stays in. */
  val MinHitPoints = 30

  /** Enemies this close make a bunker threatened; this close it is already fighting. */
  val ThreatTiles = 10
  val ReachTiles  = 6

  /** One unload per bunker this often at most. */
  val UnloadEvery = 24

  def toUnload(inside: Seq[Inside], fighting: Boolean, someoneOutside: Boolean): Seq[Int] = {
    val ready = inside.filter(m => m.stimTimer == 0 && m.hitPoints >= MinHitPoints).map(_.id)
    if (!fighting) ready
    else if (someoneOutside) Nil
    else ready.take(1)
  }

  /** A Marine on its way back in stims first while its bunker is threatened. */
  def stimsOnTheWay(threatened: Boolean, stimTimer: Int, hitPoints: Int) =
    threatened && stimTimer == 0 && hitPoints >= MinHitPoints
}
