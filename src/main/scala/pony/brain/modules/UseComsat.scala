package pony
package brain
package modules

import pony.Upgrades.Terran._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

/**
  * Scanner sweeps only where a hidden enemy is known to be and can be killed once revealed, never on a guess:
  *   - a cloaked or burrowed enemy the game shows within our sight (the shimmer a human sees) near our units that can
  *     hit it, attackers first, then detectors such as observers;
  *   - a unit or building of ours losing hit points with no visible enemy near that could have done it (a dark templar,
  *     a lurker, a spider mine).
  * A sweep lasts about eleven seconds, so a spot just swept is not swept again.
  */
class UseComsat(universe: Universe) extends DefaultBehaviour[Comsat](universe) {
  import ScanChoice._

  private val lastHealth = mutable.HashMap.empty[Int, Int]
  private val sweeps     = mutable.ArrayBuffer.empty[(MapTilePosition, Int)]
  private var wanted     = Option.empty[Candidate]

  private def tileOf(u: bwapi.Unit) = MapTilePosition(u.getTilePosition.getX, u.getTilePosition.getY)

  override def onTick_!() = {
    super.onTick_!()
    val now = currentTick
    sweeps.filterInPlace((_, at) => now - at < SweepFrames)
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
        e.getType == bwapi.UnitType.Zerg_Lurker
      Candidate(at, if (attacker) Reason.HiddenAttacker else Reason.HiddenDetector, hunters, e.getType.toString)
    }
    // damage with no visible enemy around that could have dealt it
    val seenThreats = enemy.filter(e => e.isVisible && e.isDetected)
    val unseenHits  = mine.flatMap { u =>
      val health    = u.getHitPoints + u.getShields
      val before    = lastHealth.put(u.getID, health)
      val hit       = before.exists(_ > health)
      val explained = u.isIrradiated || u.isUnderStorm || u.isPlagued ||
        // a Terran building in the red burns down on its own
        u.getType.isBuilding && u.getType.getRace == bwapi.Race.Terran && u.getHitPoints * 3 < u.getType.maxHitPoints ||
        seenThreats.exists(e =>
          tileOf(e).distanceToIsLess(tileOf(u), VisibleCulpritRadius) &&
            (if (u.isFlying) e.getType.airWeapon != bwapi.WeaponType.None
             else e.getType.groundWeapon != bwapi.WeaponType.None ||
             e.getType == bwapi.UnitType.Protoss_Reaver || e.getType == bwapi.UnitType.Protoss_Carrier)
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
    wanted = choose(shimmering ++ unseenHits, sweeps.toSeq.map(_._1))
  }

  override protected def wrapBase(comsat: Comsat) = new SingleUnitBehaviour[Comsat](comsat, meta) {
    override def describeShort = "Scan"

    override def toOrder(what: Objective) = wanted.filter(_ => comsat.canCastNow(ScannerSweep)).map { c =>
      wanted = None
      sweeps += c.at -> currentTick
      NativeMatchEvidence.trace("comsat-sweep", s"reason=${c.reason} at=${c.at} hunters=${c.hunters} about=${c.about}")
      Orders.ScanWithComsat(comsat, c.at)
    }.toList
  }

  private def hitsGround(u: bwapi.Unit) =
    u.getType.groundWeapon != bwapi.WeaponType.None ||
      u.getType == bwapi.UnitType.Terran_Bunker && u.getLoadedUnits.size > 0

  private def hitsAir(u: bwapi.Unit) =
    u.getType.airWeapon != bwapi.WeaponType.None ||
      u.getType == bwapi.UnitType.Terran_Bunker && u.getLoadedUnits.size > 0
}

/** Where a sweep pays off, kept free of the game. */
private[pony] object ScanChoice {
  enum Reason {
    case HiddenAttacker, UnseenDamage, HiddenDetector
  }

  /** `hunters`: our units near it that could shoot the revealed enemy. */
  final case class Candidate(at: MapTilePosition, reason: Reason, hunters: Int, about: String)

  /** A sweep reveals for about this long (262 frames). */
  val SweepFrames = 262

  /** A sweep covers about this many tiles around its centre. */
  val SweepRadius = 10

  /** Our shooters this close to a hidden enemy can kill it once it is revealed. */
  val HuntRadius = 9

  /** A visible enemy this close to a damaged unit of ours may have dealt the damage. */
  val VisibleCulpritRadius = 8

  /**
    * The best place to sweep, if any: only where someone can shoot what the sweep reveals, never inside a sweep still
    * running; a hidden attacker before unexplained damage, unexplained damage before a hidden detector, more hunters
    * first.
    */
  def choose(candidates: Seq[Candidate], sweeping: Seq[MapTilePosition]): Option[Candidate] =
    candidates
      .filter(c => c.hunters > 0 && !sweeping.exists(_.distanceToIsLess(c.at, SweepRadius)))
      .sortBy(c => (c.reason.ordinal, -c.hunters))
      .headOption
}
