package pony
package brain
package modules

import scala.jdk.CollectionConverters._

/**
  * Science Vessels fly with the cruisers as their detectors. A cloaked or burrowed enemy the game shows near units of
  * ours that can shoot it draws the nearest Vessel close enough to reveal it; otherwise the Vessels keep a little
  * behind a raid's centre, or wait at the berth. Their spells (Defensive Matrix, Irradiate) come from their own
  * behaviours, which rank above this one.
  */
class EscortWithVessels(universe: Universe) extends DefaultBehaviour[ScienceVessel](universe) {
  private var hidden = Vector.empty[MapTilePosition]

  override def priority = SecondPriority(0.6)

  private def active = strategy.current.raidsWithCruisers

  override def onTick_!(): Unit = {
    super.onTick_!()
    hidden =
      if (!active) Vector.empty
      else {
        val self                = nativeGame.self()
        val all                 = nativeGame.getAllUnits.asScala.toVector
        def tile(u: bwapi.Unit) = MapTilePosition(u.getTilePosition.getX, u.getTilePosition.getY)
        val shooters            = all.filter(u =>
          u.getPlayer == self && u.isCompleted && !u.getType.isWorker &&
            (u.getType.groundWeapon != bwapi.WeaponType.None || u.getType.airWeapon != bwapi.WeaponType.None)
        ).map(tile)
        all.filter(e => e.getPlayer.isEnemy(self) && e.isVisible && !e.isDetected).map(tile)
          .filter(t => shooters.exists(_.distanceToIsLess(t, ScanChoice.HuntRadius)))
      }
  }

  override protected def wrapBase(unit: ScienceVessel) = new SingleUnitBehaviour[ScienceVessel](unit, meta) {
    override def describeShort = "Escort"

    override protected def toOrder(what: Objective) = {
      val me = this.unit
      if (!active) Nil
      else {
        val reveal = hidden.minByOpt(_.distanceSquaredTo(me.currentTile))
          .filter(_.distanceToIsMore(me.currentTile, RevealDistance))
        val follow = worldDominationPlan.cruiserRaidCentre.filter(_.distanceToIsMore(me.currentTile, 3))
        val home   = worldDominationPlan.cruiserBerth.filter(_.distanceToIsMore(me.currentTile, 4))
        reveal.orElse(follow).orElse(if (worldDominationPlan.cruiserRaidCentre.isEmpty) home else None)
          .map(Orders.MoveToTile(me, _)).toList
      }
    }
  }

  /** A Vessel detects about ten tiles around it; this close it reveals for certain without flying on top. */
  private val RevealDistance = 6
}
