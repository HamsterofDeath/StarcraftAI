package pony
package brain
package modules
package campaign

import pony.combat.ArmedBuilding
import pony.geometry.MapTilePosition
import pony.pathing.Formation
import pony.units.{GroundUnit, Mobile, SupportUnit, WorkerUnit}

import bwapi.{UnitType, WeaponType}

/**
  * A campaign attack marching in formation along its path (Formation): the long range leads, the rest keep their
  * distance behind, and the front waits for the army. When an enemy comes within reach of a member the formation lets
  * go and every unit fights as it would without one; it forms again around the army once no enemy has been near for
  * ReformFrames, and lets go for good once the front reaches the target. In the games before it the army met at the
  * edge of the enemy's defence and then charged one by one, the fastest first.
  */
class FormationMarch(route: Formation.Route, override val universe: Universe) extends HasUniverse {
  import FormationMarch._

  private var front     = Option.empty[Double]
  private var movedAt   = currentTick
  private var updatedAt = -Interval
  private var contactAt = Option.empty[Int]
  private var arrived   = false
  private var slots     = Map.empty[Int, MapTilePosition]
  private var fight     = Option.empty[MapTilePosition]
  private var middle    = Option.empty[MapTilePosition]

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
    val places   = Formation.places(fighters.map(m => m.nativeUnitId -> reach(m)))
    val depth    = Formation.depth(places)
    contact(fighters).foreach { (kind, tile) =>
      if (contactAt.forall(currentTick - _ >= ReformFrames))
        NativeMatchEvidence.trace("formation-contact", s"front=${front.map(_.round)} enemy=$kind@$tile")
      contactAt = Some(currentTick)
      fight = Some(tile)
      front = None
    }
    if (contactAt.exists(currentTick - _ < ReformFrames)) slots = Map.empty
    else {
      val at = front.getOrElse {
        val formed = Formation.formingFront(fighters.map(m => route.progressOf(m.currentTile)), depth)
        movedAt = currentTick
        NativeMatchEvidence.trace(
          "formation-form",
          s"members=${fighters.size} depth=${depth.round} front=${formed.round} route=${route.length.round}"
        )
        formed
      }
      def tiles(at: Double) =
        fighters.map(m => m.nativeUnitId -> tileFor(m, Formation.slot(route, at, places(m.nativeUnitId)))).toMap
      val now     = tiles(at)
      val keepUp  = Formation.keepsUp(fighters.map(m => m.currentTile.distanceTo(now(m.nativeUnitId))))
      val stalled = currentTick - movedAt > Formation.StallFrames
      val next    = if (keepUp || stalled) at + Formation.StepTiles else at
      if (next != at) movedAt = currentTick
      if (stalled) NativeMatchEvidence.trace("formation-stall", s"front=${at.round} members=${fighters.size}")
      if (next - depth / 2 >= route.length) {
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

  /** A ground unit's slot must be walkable and in sight of the route, else it stands on the route. */
  private def tileFor(m: Mobile, slot: Formation.Point) = {
    val walk = mapLayers.rawWalkableMap
    val tile = MapTilePosition.shared(
      slot._1.round.toInt.max(0).min(walk.cols - 1),
      slot._2.round.toInt.max(0).min(walk.rows - 1)
    )
    lazy val nearest = onRoute(route.progressOf(tile))
    m match {
      case _: GroundUnit if !walk.free(tile) || !walk.connectedByLine(tile, nearest) =>
        if (walk.free(nearest)) nearest else walk.nearestFree(nearest).getOrElse(nearest)
      case _ => tile
    }
  }

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
}
