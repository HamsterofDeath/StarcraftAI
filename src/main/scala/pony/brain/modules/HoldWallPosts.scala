package pony
package brain
package modules

import pony.Upgrades.Terran._

import scala.collection.mutable

/**
  * Behind a sealed wall every ground fighter holds a post instead of milling about: Marines and other foot soldiers
  * in a row close behind the wall so they shoot over it, Vultures a little further back, Siege Tanks sieged far
  * enough back that whatever attacks the wall cannot reach them while they still cover the wall and the ground in
  * front of it. A unit leaves its post only when it is hit itself or the enemy is inside the main. From time to time
  * a Dropship carries a Vulture out to mine the approach in front of the wall and brings it back.
  */
class HoldWallPosts(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
  import WallPosts._

  private val assigned    = mutable.HashMap.empty[Int, MapTilePosition]
  private var posts       = Map.empty[Role, Vector[MapTilePosition]]
  private var approach    = Vector.empty[MapTilePosition]
  private var homeTile    = Option.empty[MapTilePosition]
  private var plannedAt   = -1
  private var enemyInside = false
  private val miners      = new Employer[Vulture](universe)
  private var lastMineRun = -MineRunEvery

  override def priority = PostPriority

  override def canControl(u: WrapsUnit) =
    super.canControl(u) && u.isInstanceOf[GroundUnit] && u.isFigher && !u.isInstanceOf[WorkerUnit]

  private def active = strategy.current.sealsMain && ferryManager.sealing

  private def inside(tile: MapTilePosition) = ferryManager.wallSide(tile).contains(true)

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (!active) {
      if (posts.nonEmpty) { posts = Map.empty; assigned.clear() }
      return
    }
    if (posts.isEmpty || currentTick - plannedAt > ReplanFrames) plan()
    val gone = assigned.keySet.filterNot(id => ownUnits.byId(id).exists(_.isInGame))
    assigned --= gone
    enemyInside = enemies.allByType[GroundUnit].exists(e =>
      e.isInGame && !e.isInstanceOf[WorkerUnit] && !e.isHarmlessNow && inside(e.currentTile)
    )
    sendMiner()
  }

  /** Posts from the wall's footprint and the direction into the main; recomputed now and then. */
  private def plan(): Unit = bases.mainBase.foreach { main =>
    val wall = universe.pluginByType[WallWithDepots].footprintTiles
    if (wall.nonEmpty) {
      plannedAt = currentTick
      val cc = main.mainBuilding.centerTile
      homeTile = Some(cc)
      val layout                                         = WallPosts.layout(wall, cc)
      val taken                                          = mutable.HashSet.empty[MapTilePosition]
      val grid                                           = mapLayers.freeWalkableIgnoringMobiles
      def snap(p: (Double, Double), wantInside: Boolean) = {
        val tile = MapTilePosition.shared(p._1.round.toInt max 0, p._2.round.toInt max 0)
        (Iterator(tile) ++ grid.spiralAround(tile, 4).iterator).find { t =>
          grid.containsAndFree(t) && !taken(t) && ferryManager.wallSide(t).contains(wantInside)
        }.map { t => taken += t; t }
      }
      posts = Role.values.map(r => r -> layout.posts(r).flatMap(snap(_, wantInside = true))).toMap
      approach = layout.approach.flatMap(snap(_, wantInside = false))
      assigned.clear()
      NativeMatchEvidence.trace(
        "wall-posts",
        Role.values.map(r => s"$r=${posts(r).size}").mkString(" ") + s" approach=${approach.mkString(",")}"
      )
    }
  }

  private def roleOf(u: Mobile): Role = u match {
    case _: Tank    => Role.Tank
    case _: Vulture => Role.Vulture
    case _          => Role.Infantry
  }

  private def postOf(u: Mobile): Option[MapTilePosition] = assigned.get(u.nativeUnitId).orElse {
    val free = posts.getOrElse(roleOf(u), Vector.empty).filterNot(assigned.values.toSet)
    free.headOption.map { p => assigned(u.nativeUnitId) = p; p }
  }

  /** Every so often, while mines are researched and too few lie in front of the wall, one Vulture goes out to lay them. */
  private def sendMiner(): Unit = for (home <- homeTile if approach.nonEmpty) {
    val running = unitManager.allJobsByType[MineWallApproach].exists(j => !j.failedOrObsolete && !j.isFinished)
    val laid    = ownUnits.allByType[SpiderMine].count(m => approach.exists(_.distanceToIsLess(m.currentTile, 4)))
    if (
      !running && laid < approach.size * 2 && upgrades.hasResearched(SpiderMines) &&
      currentTick - lastMineRun > MineRunEvery
    ) {
      val request = UnitJobRequest.idleOfType(miners, classOf[Vulture], 1, Priority.Supply)
        .withOnlyAccepting(v => v.spiderMineCount > 0 && inside(v.currentTile))
      unitManager.request(request, buildIfNoneAvailable = false).units.foreach { v =>
        lastMineRun = currentTick
        assigned -= v.nativeUnitId
        miners.assignJob_!(new MineWallApproach(v, approach, home, miners))
        NativeMatchEvidence.trace(
          "wall-mine-run",
          s"vulture=${v.nativeUnitId} spots=${approach.mkString(",")} laid=$laid"
        )
      }
    }
  }

  override protected def wrapBase(unit: Mobile) = new SingleUnitBehaviour[Mobile](unit, meta) {
    override def describeShort = "Wall post"

    override protected def toOrder(what: Objective) = {
      val me = this.unit
      if (!active || enemyInside) Nil
      else postOf(me).toList.flatMap { post =>
        val away = me.currentTile.distanceToIsMore(post, 1)
        me match {
          // a building planned on the post since: give it up, the next tick brings another
          case _ if !mapLayers.blockedByPlannedBuildings.free(post) =>
            assigned -= me.nativeUnitId
            Nil
          case tank: Tank =>
            val canSiege = upgrades.hasResearched(TankSiegeMode)
            if (away && tank.isSieged) List(Orders.TechOnSelf(tank, TankSiegeMode))
            else if (away) List(Orders.MoveToTile(tank, post))
            else if (canSiege && !tank.isSieged) List(Orders.TechOnSelf(tank, TankSiegeMode))
            else List(Orders.HoldPosition(tank))
          // hit itself: the ranged micro answers
          case c: CanDie if c.isBeingAttacked => Nil
          case _ if away                      => List(Orders.MoveToTile(me, post))
          case _                              => List(Orders.HoldPosition(me))
        }
      }
    }
  }
}
