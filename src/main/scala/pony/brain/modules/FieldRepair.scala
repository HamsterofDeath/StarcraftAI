package pony
package brain
package modules

/**
  * One SCV of a field crew: while a raid holds still with hurt cruisers, it goes to the spot under the fleet (by
  * dropship across a sealed wall, on foot otherwise) and mends the most hurt cruiser near it until that one is whole;
  * repairs of several SCVs stack, so a mended standing fleet is hard to kill. When the raid moves on or ends it goes
  * back to the berth.
  */
private[pony] class FieldCrewDuty(
    worker: SCV,
    spot: () => Option[MapTilePosition],
    home: MapTilePosition,
    owner: Employer[SCV]
) extends UnitWithJob[SCV](owner, worker, Priority.Supply) with FerrySupport[SCV] {
  import FieldRepair._

  private val startedAt = currentTick
  private var mending   = Option.empty[Battlecruiser]

  override def shortDebugString = "Cruiser field crew"
  override def everyNth         = 11

  override protected def ferryDropTarget = spot().orElse(Some(home))

  private def atHome = ferryManager.wallSide(worker.currentTile).contains(true) ||
    !worker.currentTile.distanceToIsMore(home, 6)

  override def isFinished = spot().isEmpty && worker.onGround && atHome

  override def jobHasFailedWithoutDeath = currentTick - startedAt > MaxFrames

  override def ordersForTick = spot() match {
    case Some(field) if worker.onGround && !worker.currentTile.distanceToIsMore(field, ArrivedTiles) =>
      def near(c: Battlecruiser) =
        c.isInGame && c.isDamaged && !c.currentTile.distanceToIsMore(worker.currentTile, ReachTiles)
      mending = mending.filter(near).orElse(ownUnits.allByType[Battlecruiser].filter(near).minByOpt(_.percentageHPOk))
      mending.map(c => Orders.RepairUnit(worker, c)).toSeq
    // on foot when no wall stands between: the ferry logic above takes over otherwise
    case Some(field) if worker.onGround     => Seq(Orders.MoveToTile(worker, field))
    case None if worker.onGround && !atHome => Seq(Orders.MoveToTile(worker, home))
    case _                                  => Nil
  }
}

/** When a raid wants its field crew, kept free of the game. */
private[pony] object FieldRepair {

  /** SCVs of a field crew. */
  val CrewSize = 3

  /** A raid whose centre moved less than this many tiles over StationaryFrames holds still. */
  val StationaryTiles  = 3
  val StationaryFrames = 24 * 10

  /** Cruisers below this share of their hit points count as hurt; this many hurt call the crew. */
  val HurtBelow  = 0.7
  val HurtNeeded = 2

  /** Enemy ground fighters this close to the spot keep the crew away. */
  val SafeTiles = 8

  val ArrivedTiles = 4
  val ReachTiles   = 8
  val MaxFrames    = 24 * 300

  /** Whether the raid calls its crew: holding still, enough cruisers hurt, no enemy ground fighters at the spot. */
  def wanted(stationary: Boolean, hurt: Int, enemyGroundNear: Boolean) =
    stationary && hurt >= HurtNeeded && !enemyGroundNear

  /** Whether the centre stayed within StationaryTiles over the trail (oldest first) covering StationaryFrames. */
  def stationary(trail: Seq[(Int, MapTilePosition)], now: Int): Boolean =
    trail.headOption.exists(_._1 <= now - StationaryFrames) && {
      val recent = trail.filter(_._1 >= now - StationaryFrames).map(_._2)
      recent.forall(a => recent.forall(b => !a.distanceToIsMore(b, StationaryTiles)))
    }
}
