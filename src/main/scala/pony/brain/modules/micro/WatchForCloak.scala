package pony
package brain
package modules
package micro

import pony.brain.modules.wall.WallWithDepots
import pony.geometry.MapTilePosition
import pony.units.WrapsUnit

import bwapi.UnitType

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * What we know of hidden enemies, dark templar above all, for the scanner (UseComsat) and for building detection
  * early:
  *   - a cloaked or burrowed enemy the game shows within our sight (the shimmer a human sees);
  *   - a unit or building of ours losing hit points with nothing visible near that could have done it, and none of our
  *     own fire either: the wall depot our army knocks down, the splash of our sieged tanks and spider mines;
  *   - an enemy that cloaks, or the building that makes it, even while detected.
  * A hidden attacker, unexplained damage or such an enemy makes a cloak threat for the rest of the game: comsat and
  * turrets come at once (TerranDefaults), and the campaign brings science vessels along later. Unseen damage used to be
  * swept in every game, mostly on the wall depot our own army was demolishing, which spent the energy dark templar
  * needed.
  */
class WatchForCloak(universe: Universe) extends OrderlessAIModule[WrapsUnit](universe) {
  import ScanChoice._
  import WatchForCloak._

  private val lastHealth = mutable.HashMap.empty[Int, Int]
  private var seen       = Vector.empty[Candidate]
  private var since      = Option.empty[Int]
  private var detection  = (0, 0, 0)

  /** Where a sweep would reveal something now. */
  def candidates: Seq[Candidate] = seen

  /** A hidden enemy has been seen or has hurt us this game. */
  def threat: Boolean = since.isDefined

  private def tileOf(u: bwapi.Unit) = MapTilePosition(u.getTilePosition.getX, u.getTilePosition.getY)

  override def onTick_!(): Unit = {
    super.onTick_!()
    val self     = nativeGame.self()
    val all      = nativeGame.getAllUnits.asScala.toVector
    val enemy    = all.filter(u => u.getPlayer.isEnemy(self))
    val mine     = all.filter(u => u.getPlayer == self && u.isCompleted)
    val shooters = mine.filter(u => !u.getType.isWorker && (hitsGround(u) || hitsAir(u)))
    // a cloaked or burrowed enemy in sight: the game reports it, but nothing of ours detects it
    val shimmering = enemy.filter(e => e.isVisible && !e.isDetected).map { e =>
      val at      = tileOf(e)
      val hunters = shooters.count(s =>
        tileOf(s).distanceToIsLess(at, HuntRadius) && (if (e.isFlying) hitsAir(s) else hitsGround(s))
      )
      val attacker = e.getType.groundWeapon != bwapi.WeaponType.None || e.getType.airWeapon != bwapi.WeaponType.None ||
        e.getType == UnitType.Zerg_Lurker
      Candidate(at, if (attacker) Reason.HiddenAttacker else Reason.HiddenDetector, hunters, e.getType.toString)
    }
    // damage with no visible enemy around that could have dealt it, and none of ours
    val seenThreats = enemy.filter(e => e.isVisible && e.isDetected)
    val wall        = universe.pluginByType[WallWithDepots]
    val sieged      = mine.filter(_.getType == UnitType.Terran_Siege_Tank_Siege_Mode).map(tileOf)
    val mines       = mine.filter(_.getType == UnitType.Terran_Vulture_Spider_Mine).map(tileOf)
    val unseenHits  = mine.flatMap { u =>
      val health    = u.getHitPoints + u.getShields
      val before    = lastHealth.put(u.getID, health)
      val hit       = before.exists(_ > health)
      val explained = u.isIrradiated || u.isUnderStorm || u.isPlagued ||
        // a Terran building in the red burns down on its own
        u.getType.isBuilding && u.getType.getRace == bwapi.Race.Terran && u.getHitPoints * 3 < u.getType.maxHitPoints ||
        wall.demolishing(u.getID) || ownFire(tileOf(u), sieged, mines) ||
        seenThreats.exists(e =>
          tileOf(e).distanceToIsLess(tileOf(u), VisibleCulpritRadius) &&
            (if (u.isFlying) e.getType.airWeapon != bwapi.WeaponType.None
             else e.getType.groundWeapon != bwapi.WeaponType.None ||
             e.getType == UnitType.Protoss_Reaver || e.getType == UnitType.Protoss_Carrier)
        )
      Option.when(hit && !explained) {
        val at = tileOf(u)
        Candidate(
          at,
          Reason.UnseenDamage,
          shooters.count(s => tileOf(s).distanceToIsLess(at, HuntRadius)),
          u.getType.toString
        )
      }
    }
    lastHealth.filterInPlace((id, _) => mine.exists(_.getID == id))
    seen = shimmering ++ unseenHits
    if (since.isEmpty) {
      val hidden =
        seen.find(_.reason != Reason.HiddenDetector).map(c => s"reason=${c.reason} about=${c.about} at=${c.at}")
      val tech = enemy.find(e => e.isVisible && CloakTech(e.getType))
        .map(e => s"reason=CloakTech about=${e.getType} at=${tileOf(e)}")
      hidden.orElse(tech).foreach { why =>
        since = Some(currentTick)
        NativeMatchEvidence.trace("cloak-threat", why)
      }
    }
    // when our detection comes, for the traces
    val now = (
      mine.count(_.getType == UnitType.Terran_Comsat_Station),
      mine.count(_.getType == UnitType.Terran_Missile_Turret),
      mine.count(_.getType == UnitType.Terran_Science_Vessel)
    )
    if (now != detection) {
      detection = now
      NativeMatchEvidence.trace("detection", s"comsats=${now._1} turrets=${now._2} vessels=${now._3}")
    }
  }

  private def hitsGround(u: bwapi.Unit) =
    u.getType.groundWeapon != bwapi.WeaponType.None ||
      u.getType == UnitType.Terran_Bunker && u.getLoadedUnits.size > 0

  private def hitsAir(u: bwapi.Unit) =
    u.getType.airWeapon != bwapi.WeaponType.None ||
      u.getType == UnitType.Terran_Bunker && u.getLoadedUnits.size > 0
}

object WatchForCloak {

  /** Enemies that are hidden themselves or hide others, and the buildings that make the dark templar. */
  val CloakTech: Set[UnitType] = Set(
    UnitType.Protoss_Dark_Templar,
    UnitType.Protoss_Templar_Archives,
    UnitType.Protoss_Arbiter,
    UnitType.Protoss_Arbiter_Tribunal,
    UnitType.Zerg_Lurker,
    UnitType.Zerg_Lurker_Egg,
    UnitType.Terran_Ghost
  )
}
