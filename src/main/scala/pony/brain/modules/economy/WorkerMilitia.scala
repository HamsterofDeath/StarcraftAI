package pony
package brain
package modules
package economy

import pony.brain.jobs.{Employer, Interruptable, UnitWithJob}
import pony.brain.requests.{UnitJobRequest, UnitRequest}
import pony.geometry.MapTilePosition
import pony.units.{MainBuilding, Mobile, SCV, WorkerUnit}

import bwapi.UnitType

import scala.collection.mutable

/**
  * SCVs fight a small raid at a base that its fighters cannot answer, and go back to mining once it is over. In game 1
  * vs Zerg on af1b88d two Zerglings came at frame 3600 and five by 5400, before the first Marine; the SCVs went on
  * mining while the Zerglings killed them one by one, and the main fell by minute 8.
  */
class WorkerMilitia(universe: Universe) extends OrderlessAIModule[SCV](universe) {
  import WorkerMilitia._

  private val militia = new Employer[SCV](universe)
  // the raiders seen at each base, by the centre of its main building
  private var raids   = Map.empty[MapTilePosition, Vector[Mobile]]
  private var grounds = Map.empty[MapTilePosition, mutable.BitSet]
  private var wanted  = Map.empty[MapTilePosition, Int]
  private var reports = Map.empty[MapTilePosition, String]

  /** Whether the base still calls for its militia. */
  def calls(home: MapTilePosition) = wanted.getOrElse(home, 0) > 0

  /** The raider of `home` nearest the worker. */
  def targetFor(worker: SCV, home: MapTilePosition): Option[Mobile] =
    raids.getOrElse(home, Vector.empty).iterator.filter(_.isInGame)
      .minByOpt(_.currentTile.distanceSquaredTo(worker.currentTile))

  // every heavy tick: an ifNth on top of the heavy cadence comes round only every 11 * 24 frames
  override def onTick_!(): Unit = update()

  private def update(): Unit = {
    val mains = bases.allBases.iterator.map(_.mainBuilding)
      .filter(b => b.isInGame && !b.isBeingCreated && !b.isFloating).toVector.distinctBy(_.centerTile)
    val armed = enemies.allCompletedMobiles.iterator.filter { e =>
      e.isInGame && !e.isInstanceOf[WorkerUnit] && !e.nativeUnit.isFlying && e.nativeUnit.isVisible &&
      e.nativeUnit.isDetected && !e.isHarmlessNow && e.nativeUnit.getType.groundWeapon.damageAmount > 0
    }.toVector
    val found = mains.flatMap { main =>
      val home = main.centerTile
      val near = armed.filter(e => home.distanceToIsLess(e.currentTile, Reach))
      if (near.isEmpty) None
      else {
        val ground  = homeGround(main)
        val raiders = near.filter(e => reaches(ground, e.currentTile))
        Option.when(raiders.nonEmpty)(home -> (raiders, ground))
      }
    }.toMap
    raids = found.map((home, raid) => home -> raid._1)
    grounds = found.map((home, raid) => home -> raid._2)
    wanted = raids.map { (home, raiders) =>
      val fighters = ownUnits.allMobilesWithWeapons.count(m =>
        m.isInGame && !m.isBeingCreated && !m.isInstanceOf[WorkerUnit] &&
          m.currentTile.distanceToIsLess(home, Reach + 4)
      )
      home -> WorkerMilitia.wanted(raiders.map(r => r.nativeUnit.getType), fighters)
    }
    wanted.foreach { (home, count) =>
      val serving = unitManager.allJobsByType[MilitiaDuty].count(j =>
        j.home == home && !j.failedOrObsolete && !j.isFinished
      )
      val hired = if (count > serving) hire(home, count - serving) else 0
      val by    = raids(home).groupBy(_.nativeUnitType.toString.replaceAll("^[A-Za-z]+_", "")).toVector
        .map((k, v) => s"$k*${v.size}").sorted.mkString(",")
      val report = s"home=$home raiders=$by wanted=$count"
      if (hired > 0 || !reports.get(home).contains(report)) {
        NativeMatchEvidence.trace("militia", s"$report serving=$serving hired=$hired")
        reports += home -> report
      }
    }
    reports = reports.filter((home, _) => wanted.contains(home))
  }

  private def hire(home: MapTilePosition, count: Int) = {
    val ground  = grounds.getOrElse(home, mutable.BitSet.empty)
    val request = UnitJobRequest.idleOfType(militia, classOf[SCV], count, Priority.Supply)
      .withOnlyAccepting { w =>
        val job = unitManager.jobOf(w)
        w.isInGame && !w.isBeingCreated && w.onGround && w.nativeUnit.getHitPoints >= MinHitPoints &&
        reaches(ground, w.currentTile) && (job.isIdle || job.isInstanceOf[GatherMineralsAtSinglePatch])
      }.withRequest(_.withCherryPicker_!(UnitRequest.CherryPickers.cherryPickWorkerByDistance[SCV](home)()))
    val hired = unitManager.request(request, buildIfNoneAvailable = false).units.toVector
    hired.foreach(w => militia.assignJob_!(new MilitiaDuty(w, home, this, militia)))
    hired.size
  }

  /**
    * The tiles within reach of a base's main building that ground units walk to from beside it, around our buildings:
    * a closed wall keeps the raiders outside it out of the militia's reach, and its SCVs inside.
    */
  private def homeGround(main: MainBuilding) = {
    val grid                        = mapLayers.freeWalkableIgnoringMobiles
    val home                        = main.centerTile
    val seen                        = mutable.BitSet.empty
    val queue                       = mutable.Queue.empty[(Int, Int)]
    def visit(x: Int, y: Int): Unit = {
      val dx = x - home.x
      val dy = y - home.y
      if (grid.inBounds(x, y) && !seen(x + y * grid.cols) && grid.free(x, y) && dx * dx + dy * dy < GroundTiles) {
        seen += x + y * grid.cols
        queue += x -> y
      }
    }
    val a = main.area
    for (x <- a.upperLeft.x - 1 to a.lowerRight.x + 1; y <- a.upperLeft.y - 1 to a.lowerRight.y + 1) visit(x, y)
    // only straight steps: a diagonal step slips between two buildings that touch at a corner
    while (queue.nonEmpty) {
      val (x, y) = queue.dequeue()
      visit(x + 1, y)
      visit(x - 1, y)
      visit(x, y + 1)
      visit(x, y - 1)
    }
    seen
  }

  /** Whether a unit on the tile stands on the ground: units stand on the edges of blocked tiles too. */
  private def reaches(ground: mutable.BitSet, t: MapTilePosition) = {
    val cols = mapLayers.freeWalkableIgnoringMobiles.cols
    (-1 to 1).exists(dx => (-1 to 1).exists(dy => ground(t.x + dx + (t.y + dy) * cols)))
  }
}

object WorkerMilitia {

  /**
    * How far from the centre of a base's main building raiders are met: across the main up to its wall, where the
    * Zerglings of game 1 vs Zerg on af1b88d killed the SCV building the gate barracks, fifteen tiles from it.
    */
  val Reach = 20

  /** The squared radius of the ground searched for the raiders' way in: a little beyond Reach. */
  val GroundTiles = (Reach + 2) * (Reach + 2)

  /** SCVs this hurt stay out of the fight. */
  val MinHitPoints = 20

  /** A raid bigger than this is an army that SCVs only die against. */
  val MaxRaiders = 8

  /** SCVs that beat one raider: three hold a Zergling, a Zealot takes six. */
  def scvsFor(t: UnitType) =
    if (t == UnitType.Zerg_Zergling) 3 else if (t == UnitType.Protoss_Zealot) 6 else 4

  /** SCVs a raid calls for, less what the fighters near the base answer. */
  def wanted(raiders: Seq[UnitType], fighters: Int) =
    if (raiders.isEmpty || raiders.size > MaxRaiders) 0 else (raiders.map(scvsFor).sum - 3 * fighters) max 0
}

/** One SCV of the militia fights the raider nearest it, and goes back to work once its base no longer calls for it. */
private[pony] class MilitiaDuty(worker: SCV, val home: MapTilePosition, militia: WorkerMilitia, owner: Employer[SCV])
    extends UnitWithJob[SCV](owner, worker, Priority.Supply) with Interruptable[SCV] {
  private def target            = militia.targetFor(worker, home)
  override def shortDebugString = s"Militia at $home"
  override def isFinished       =
    !militia.calls(home) || target.isEmpty || worker.currentTile.distanceToIsMore(home, WorkerMilitia.Reach + 6)
  override def jobHasFailedWithoutDeath = !worker.nativeUnit.exists || worker.isDead
  override def everyNth                 = 7
  override def ordersForTick            = target.map(Orders.AttackUnit(worker, _)).toSeq
}
