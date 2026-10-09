package pony
package brain
package modules

import scala.collection.mutable

/** Spreads tanks, vultures and goliaths evenly over the map instead of one death ball. */
class CarpetSpread(universe: Universe) extends OrderlessAIModule[Mobile](universe) {
  private val assignments   = mutable.Map.empty[Int, MapTilePosition]
  private var reportedPosts = false

  def postOf(unitId: Int): Option[MapTilePosition] = assignments.get(unitId)

  private def carpet = strategy.current.isInstanceOf[Strategy.TerranCarpet]
  private def wall   = universe.pluginByType[WallWithDepots]

  private val pocket = oncePerTick {
    bases.mainBase.flatMap(home => strategicMap.defenseLineOf(home.mainBuilding.tilePosition))
  }

  /** A sealed wall turns the home plateau into a pocket ground units cannot leave. */
  private def insidePocket(t: MapTilePosition) = wall.complete && pocket.get.exists(_.defended.free(t))

  private val plannedPosts = oncePerTick {
    val home       = bases.mainBase.flatMap(_.resourceArea).map(_.uniqueId)
    val candidates = strategicMap.resources.filterNot(a => home.contains(a.uniqueId))
      .map(_.nearbyFreeTile).toVector.filter(mapLayers.rawWalkableMap.insideBounds)
    val count = if (CarpetQuotas.maxPosts > 0) CarpetQuotas.maxPosts else candidates.size
    CarpetPosts.order(candidates, count)
  }

  private def kind(m: Mobile) = m match {
    case _: Tank    => 0
    case _: Vulture => 1
    case _          => 2
  }

  private def quota(kind: Int) = kind match {
    case 0 => CarpetQuotas.tanksPerPost
    case 1 => CarpetQuotas.vulturesPerPost
    case _ => CarpetQuotas.goliathsPerPost
  }

  override def onTick_!(): Unit = {
    if (!carpet || currentTick < 31 || currentTick % Primes.prime31.i != 0) return
    // The wall protects the opening; once a real army stands on a second base it becomes the
    // defense, and one depot is demolished so the army can reach its map-wide posts.
    if (wall.complete && !wall.gateOpen) {
      val operational = universe.pluginByType[ManageMiningAtBases].secondBaseEstablished
      val army        = ownUnits.allMobilesWithWeapons.count(m =>
        m.isInGame && !m.isBeingCreated &&
          m.isFigher && !m.isInstanceOf[WorkerUnit]
      )
      if (operational && army >= CarpetQuotas.gateFighters) wall.openGate_!()
    }
    val posts = plannedPosts.get
    if (posts.nonEmpty && !reportedPosts) {
      NativeMatchEvidence.trace("carpet-posts", s"posts=${posts.size} at=${posts.mkString(",")}")
      reportedPosts = true
    }
    val units = ownUnits.allMobilesWithWeapons.filter(m =>
      m.isInGame && !m.isBeingCreated &&
        (m.isInstanceOf[Tank] || m.isInstanceOf[Vulture] || m.isInstanceOf[Goliath])
    )
      .groupBy(_.nativeUnitId).values.map(_.head).toVector.sortBy(_.nativeUnitId)
    assignments.filterInPlace((id, post) => posts.contains(post) && units.exists(_.nativeUnitId == id))
    val counts = mutable.Map.empty[(MapTilePosition, Int), Int]
    assignments.foreach { case (id, post) =>
      units.find(_.nativeUnitId == id).foreach(u => counts((post, kind(u))) = counts.getOrElse((post, kind(u)), 0) + 1)
    }
    val home      = bases.mainBase.map(_.mainBuilding.tilePosition)
    val homeQuota = if (wall.complete) 0 else CarpetQuotas.homeGuards
    val eligible  = units.filterNot(u => assignments.contains(u.nativeUnitId))
      .sortBy(u => (home.map(h => u.currentTile.distanceSquaredTo(h)).getOrElse(0), u.nativeUnitId))
      .filterNot(u => insidePocket(u.currentTile))
      .drop(homeQuota)
    eligible.foreach { u =>
      val k = kind(u)
      posts.minByOpt { p =>
        val existing = counts.getOrElse((p, k), 0)
        (if (existing >= quota(k)) 1 else 0, existing, u.currentTile.distanceSquaredTo(p))
      }.foreach { p =>
        assignments(u.nativeUnitId) = p
        counts((p, k)) = counts.getOrElse((p, k), 0) + 1
        NativeMatchEvidence.trace("carpet-assign", s"unit=${u.nativeUnitId} post=$p")
      }
    }
    if (currentTick % (31 * 16) == 0) {
      NativeMatchEvidence.trace(
        "strategy-carpet",
        s"posts=${posts.size} units=${units.size} assigned=${assignments.size} homeGuards=$homeQuota wallComplete=${wall.complete} wallRefused=${wall.refused}"
      )
    }
  }
}
