package pony
package brain
package modules
package campaign

import pony.combat.ArmedBuilding
import pony.geometry.MapTilePosition
import pony.pathing.{Formation, Paths}
import pony.units.{GroundUnit, Mobile, SupportUnit, WorkerUnit}
import pony.util.BWFuture

import bwapi.{UnitType, WeaponType}

import scala.util.Try

/**
  * A campaign attack marching in formation along its path (Formation): the long range leads, the rest keep their
  * distance behind, and the front waits for the army. When an enemy comes within reach of a member the formation lets
  * go and every unit fights as it would without one; it forms again around the army once no enemy has been near for
  * ReformFrames, and lets go for good once the front reaches the target. Reinforcements take their slots too but hold
  * nobody up until they have caught up. In the games before it the army met at the edge of the enemy's defence and
  * then charged one by one, the fastest first.
  *
  * The army leaves our bases on its own and plans its route once half of it stands HomeTiles away from them: the
  * attack's own path is planned at the launch, while the wall still seals the main, and so runs to the corner of the
  * main nearest the target and on in a straight line through the cliffs.
  */
class FormationMarch(target: MapTilePosition, override val universe: Universe) extends HasUniverse {
  import FormationMarch._

  private var front     = Option.empty[Double]
  private var movedAt   = currentTick
  private var updatedAt = -Interval
  private var contactAt = Option.empty[Int]
  private var arrived   = false
  private var slots     = Map.empty[Int, MapTilePosition]
  private var fight     = Option.empty[MapTilePosition]
  private var middle    = Option.empty[MapTilePosition]
  private var joined    = Set.empty[Int]
  private var stalls    = 0
  private val since     = currentTick
  private var planned   = Option.empty[BWFuture[Option[Paths]]]
  private var marching  = Option.empty[Formation.Route]

  private def route = marching.get

  /**
    * Where a member goes: its slot while the formation holds (a medic, without one, to the formation's middle), the
    * enemy that broke it while the army fights, and none once the front has reached the target.
    */
  def destinationOf(m: Mobile): Option[MapTilePosition] =
    if (slots.nonEmpty) slots.get(m.nativeUnitId).orElse(middle)
    else fight.filter(_ => contactAt.exists(currentTick - _ < ReformFrames))

  def update(force: Iterable[Mobile]): Unit = if (!arrived && currentTick - updatedAt >= Interval) {
    updatedAt = currentTick
    val fighters = force.filter(m => m.isInGame && !m.isInstanceOf[SupportUnit]).toVector
    if (fighters.nonEmpty) march(fighters)
  }

  private def march(fighters: Vector[Mobile]): Unit = {
    val places = Formation.places(fighters.map(m => m.nativeUnitId -> reach(m)))
    val depth  = Formation.depth(places)
    contact(fighters).foreach { (kind, tile) =>
      if (contactAt.forall(currentTick - _ >= ReformFrames))
        NativeMatchEvidence.trace("formation-contact", s"front=${front.map(_.round)} enemy=$kind@$tile")
      contactAt = Some(currentTick)
      fight = Some(tile)
      front = None
    }
    if (contactAt.exists(currentTick - _ < ReformFrames)) slots = Map.empty
    else if (marching.isEmpty) plan(fighters)
    else {
      val forming = front.isEmpty
      val at      =
        front.getOrElse(Formation.formingFront(fighters.map(m => route.progressOf(m.currentTile)), depth))
      def tiles(at: Double) =
        fighters.map(m => m.nativeUnitId -> tileFor(m, Formation.slot(route, at, places(m.nativeUnitId)))).toMap
      val now                                   = tiles(at)
      val apart                                 = fighters.map(m => m -> m.currentTile.distanceTo(now(m.nativeUnitId)))
      def caughtUp(m: Mobile, distance: Double) =
        Formation.caughtUp(route.progressOf(m.currentTile), at - places(m.nativeUnitId)._1, distance)
      if (forming) {
        // the army is who stands near the formation as it forms; whoever is further away catches up and joins
        // the army once at its slot, without holding it up: reinforcements walk out of the base behind it
        val near = apart.collect { case (m, d) if d <= GatherTiles => m.nativeUnitId }.toSet
        joined = if (near.nonEmpty) near else fighters.map(_.nativeUnitId).toSet
        movedAt = currentTick
        NativeMatchEvidence.trace(
          "formation-form",
          s"members=${fighters.size} army=${joined.size} depth=${depth.round} front=${at.round} " +
            s"route=${route.length.round} after=${currentTick - since} via=${via.mkString(",")}"
        )
      }
      joined ++= apart.collect { case (m, d) if caughtUp(m, d) => m.nativeUnitId }
      val army    = apart.filter((m, _) => joined(m.nativeUnitId))
      val behind  = army.filterNot((m, d) => caughtUp(m, d))
      val keepUp  = Formation.keepsUp(army.map((m, d) => caughtUp(m, d)))
      val stalled = currentTick - movedAt > Formation.StallFrames
      val next    = if (keepUp || stalled) at + Formation.StepTiles else at
      if (next != at) movedAt = currentTick
      if (keepUp) stalls = 0 else if (stalled) stalls += 1
      if (stalled) {
        // who holds the army up, and which behaviour orders them instead
        val lagging = behind.sortBy(-_._2).take(6).map { (m, d) =>
          val n  = m.nativeUnit
          val by = unitManager.jobOf(m).shortDebugString.stripPrefix("[BG] ").replace(',', ';')
          s"${m.nativeUnitId}:${n.getType.toString.replaceAll("^[A-Za-z]+_", "")}@${m.currentTile}" +
            s">${now(m.nativeUnitId)}=${d.round}:${n.getOrder}:$by"
        }
        NativeMatchEvidence.trace(
          "formation-stall",
          s"front=${at.round} members=${fighters.size} army=${army.size} " +
            s"behind=${behind.size} stalls=$stalls lagging=${lagging.mkString(",")}"
        )
      }
      if (stalls >= Formation.GiveUpStalls)
        abandon(s"front=${at.round} members=${fighters.size} army=${army.size} behind=${behind.size}")
      else if (next - depth / 2 >= route.length) {
        arrived = true
        slots = Map.empty
        NativeMatchEvidence.trace("formation-arrived", s"members=${fighters.size} route=${route.length.round}")
      } else {
        front = Some(next)
        slots = if (next == at) now else tiles(next)
        middle = Some(onRoute(next - depth / 2))
      }
    }
  }

  /**
    * Until half the army stands HomeTiles from all our bases every member attacks towards the target on its own; then
    * the route is planned from the army's median member. A route that does not reach the target means no formation.
    */
  private def plan(fighters: Vector[Mobile]): Unit = planned match {
    case None =>
      val homes = bases.allBases.map(_.mainBuilding.centerTile)
      val away  = fighters.map(m => m -> homes.map(m.currentTile.distanceTo).minOption.getOrElse(Double.MaxValue))
      if (Formation.leftHome(away.map(_._2), HomeTiles)) {
        val from = away.sortBy(_._2).apply(away.size / 2)._1.currentTile
        planned = Some(universe.pathfinders.groundSafe.findPaths(from, target, 1))
      } else if (currentTick - since > LeaveFrames)
        abandon(s"leaving members=${fighters.size} away=${away.count(_._2 >= HomeTiles)}")
    case Some(future) if future.isDone =>
      Try(future.result).toOption.flatten.flatMap(_.paths.headOption) match {
        case Some(path) if path.solved && path.requestedTarget.distanceToIsLess(target, TargetTiles) =>
          val corners = Formation.taut(path.waypoints :+ target, mapLayers.rawWalkableMap.connectedByLine)
          marching = Some(new Formation.Route(corners))
        case other =>
          abandon(s"unrouted solved=${other.map(_.solved)} reaches=${other.map(_.requestedTarget)} target=$target")
      }
    case Some(_) => // still planning
  }

  private def abandon(why: String): Unit = {
    arrived = true
    slots = Map.empty
    NativeMatchEvidence.trace("formation-abandon", why)
  }

  /** The first visible enemy within reach of a member, if any. */
  private def contact(fighters: Seq[Mobile]) = {
    val buildings = enemies.allByType[ArmedBuilding].filter(b => b.isInGame && b.nativeUnit.isVisible)
    fighters.iterator.flatMap { m =>
      val radius = math.ceil(reach(m)).toInt + ContactTiles
      unitGrid.enemy.allInRange[Mobile](m.currentTile, radius).find(e =>
        e.nativeUnit.isVisible && !e.isInstanceOf[WorkerUnit] &&
          (e.nativeUnit.getType.canAttack || e.nativeUnit.getType.isSpellcaster)
      ).orElse(buildings.find(_.centerTile.distanceToIsLess(m.currentTile, radius)))
    }.nextOption().map(e => e.nativeUnit.getType -> e.currentTile)
  }

  /**
    * A ground unit's slot must be walkable, in sight of the route and clear of planned buildings (units step off
    * those), else it stands on the route.
    */
  private def tileFor(m: Mobile, slot: Formation.Point) = {
    val walk = mapLayers.rawWalkableMap
    val tile = MapTilePosition.shared(
      slot._1.round.toInt.max(0).min(walk.cols - 1),
      slot._2.round.toInt.max(0).min(walk.rows - 1)
    )
    lazy val nearest = onRoute(route.progressOf(tile))
    m match {
      case _: GroundUnit
          if !walk.free(tile) || !mapLayers.blockedByPlannedBuildings.free(tile) ||
            !walk.connectedByLine(tile, nearest) =>
        if (walk.free(nearest)) nearest else walk.nearestFree(nearest).getOrElse(nearest)
      case _ => tile
    }
  }

  /** The route every 16 tiles, for the traces. */
  private def via = Iterator.iterate(0.0)(_ + 16).takeWhile(_ < route.length).map(onRoute).toVector :+
    onRoute(route.length)

  private def onRoute(s: Double) = {
    val ((x, y), _) = route.at(s)
    MapTilePosition.shared(x.round.toInt, y.round.toInt)
  }

  /** The reach of a unit's weapon in tiles, the ground weapon first; a siege tank keeps its place as a tank. */
  private def reach(m: Mobile): Double = {
    val kind = m.nativeUnit.getType match {
      case UnitType.Terran_Siege_Tank_Siege_Mode => UnitType.Terran_Siege_Tank_Tank_Mode
      case other                                 => other
    }
    val weapon = if (kind.groundWeapon != WeaponType.None) kind.groundWeapon else kind.airWeapon
    if (weapon == WeaponType.None) 0.0 else nativeGame.self().weaponMaxRange(weapon) / 32.0
  }
}

object FormationMarch {

  /** Frames between two updates of the formation. */
  val Interval = 8

  /** An enemy this many tiles beyond a member's reach breaks the formation... */
  val ContactTiles = 3

  /** ...and for this long after the last one was near, every unit fights on its own. */
  val ReformFrames = 24 * 5

  /** Members this far from their slots as the formation forms catch up instead of holding the army. */
  val GatherTiles = 20

  /** The route is planned once half the army stands this far from all our bases... */
  val HomeTiles = 20.0

  /** ...unless it takes longer than this to get there, and then there is no formation. */
  val LeaveFrames = 24 * 90

  /** A planned route must end this close to the target. */
  val TargetTiles = 12
}
