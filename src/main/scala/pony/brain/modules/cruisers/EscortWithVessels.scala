package pony
package brain
package modules
package cruisers

import pony.brain.modules.micro.ScanChoice
import pony.geometry.MapTilePosition
import pony.units.ScienceVessel

import scala.jdk.CollectionConverters._

/**
  * Science Vessels are the detectors of the cruisers and of the campaign army. A cloaked or burrowed enemy the game
  * shows near units of ours that can shoot it draws the nearest Vessel close enough to reveal it; otherwise the Vessels
  * of the cruisers keep a little behind a raid's centre, or wait at the berth, and those of the campaign army stay
  * among its fighters (the attack's support units). Their spells (Defensive Matrix, Irradiate, EMP) come from their
  * own behaviours, which rank above this one.
  */
class EscortWithVessels(universe: Universe) extends DefaultBehaviour[ScienceVessel](universe) {
  private var hidden = Vector.empty[MapTilePosition]

  override def priority = SecondPriority(0.6)

  private def withCruisers = strategy.current.raidsWithCruisers

  override def onTick_!(): Unit = {
    super.onTick_!()
    hidden =
      if (ownUnits.allByType[ScienceVessel].isEmpty) Vector.empty
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
      val me     = this.unit
      val reveal = hidden.filter(_.distanceToIsLess(me.currentTile, RevealRange))
        .minByOpt(_.distanceSquaredTo(me.currentTile))
        .filter(_.distanceToIsMore(me.currentTile, RevealDistance))
      if (!withCruisers) reveal.map(Orders.MoveToTile(me, _)).toList
      else {
        val follow = worldDominationPlan.cruiserRaidCentre.filter(_.distanceToIsMore(me.currentTile, 3))
        val home   = worldDominationPlan.cruiserBerth.filter(_.distanceToIsMore(me.currentTile, 4))
        reveal.orElse(follow).orElse(if (worldDominationPlan.cruiserRaidCentre.isEmpty) home else None)
          .map(Orders.MoveToTile(me, _)).toList
      }
    }
  }

  /** A Vessel detects about ten tiles around it; this close it reveals for certain without flying on top. */
  private val RevealDistance = 6

  /** A Vessel flies to reveal what lies this close; further away it stays with its army. */
  private val RevealRange = 40
}
