package pony
package brain
package modules

import pony.Upgrades.Terran._

import scala.collection.mutable
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
        val self     = nativeGame.self()
        val all      = nativeGame.getAllUnits.asScala.toVector
        def tile(u: bwapi.Unit) = MapTilePosition(u.getTilePosition.getX, u.getTilePosition.getY)
        val shooters = all.filter(u =>
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

/**
  * The Yamato gun on the most valuable enemy in range: casters and units that shoot at air first, a target the 260
  * damage kills outright preferred. One cruiser per target.
  */
class YamatoSnipe(universe: Universe) extends DefaultBehaviour[Battlecruiser](universe) {
  import YamatoChoice._

  private val claimed = mutable.HashMap.empty[Int, Int]

  override def priority = SecondPriority(0.93)

  override def onTick_!(): Unit = {
    super.onTick_!()
    claimed.filterInPlace((_, at) => currentTick - at < ClaimFrames)
  }

  override protected def wrapBase(unit: Battlecruiser) = new SingleUnitBehaviour[Battlecruiser](unit, meta) {
    override def describeShort = "Yamato"

    override def preconditionOk = upgrades.hasResearched(CruiserGun)

    override protected def toOrder(what: Objective) = {
      val me     = this.unit
      val native = me.nativeUnit
      if (native.getOrder == bwapi.Order.FireYamatoGun) List(Orders.NoUpdate(me))
      else if (native.getEnergy < Energy) Nil
      else {
        val self = nativeGame.self()
        val near = native.getUnitsInRadius(RangePixels).asScala.toVector.filter { e =>
          e.getPlayer.isEnemy(self) && e.isVisible && e.isDetected && !claimed.contains(e.getID)
        }
        best(near.map(candidate)).flatMap(c => near.find(_.getID == c.id)).map { target =>
          claimed(target.getID) = currentTick
          NativeMatchEvidence.trace(
            "yamato",
            s"cruiser=${me.nativeUnitId} target=${target.getType} hp=${target.getHitPoints + target.getShields}"
          )
          Orders.UseTechOnUnit(me, target, bwapi.TechType.Yamato_Gun)
        }.toList
      }
    }
  }

  private def candidate(e: bwapi.Unit) = {
    val kind = e.getType
    Candidate(
      e.getID,
      kind.mineralPrice + kind.gasPrice,
      e.getHitPoints + e.getShields,
      kind.size match {
        case bwapi.UnitSizeType.Small  => 0.5
        case bwapi.UnitSizeType.Medium => 0.75
        case _                         => 1.0
      },
      kind.airWeapon != bwapi.WeaponType.None || kind == bwapi.UnitType.Protoss_Carrier,
      Casters(kind)
    )
  }

  private val Casters = Set(
    bwapi.UnitType.Protoss_High_Templar,
    bwapi.UnitType.Protoss_Arbiter,
    bwapi.UnitType.Protoss_Dark_Archon,
    bwapi.UnitType.Zerg_Defiler,
    bwapi.UnitType.Zerg_Queen,
    bwapi.UnitType.Terran_Science_Vessel,
    bwapi.UnitType.Terran_Ghost
  )
}

/** Which enemy deserves a Yamato shot, kept free of the game. */
private[pony] object YamatoChoice {

  /** `share`: how much of the explosive Yamato damage the target's size lets through. */
  final case class Candidate(id: Int, value: Int, durability: Int, share: Double, hitsAir: Boolean, caster: Boolean)

  val Damage      = 260
  val Energy      = 150
  val RangePixels = 10 * 32

  /** A cast takes a few seconds: another cruiser leaves the target alone this long. */
  val ClaimFrames = 72

  /** Below this a shot is not worth its 150 energy. */
  val MinScore = 150.0

  val CasterBonus = 300

  def score(c: Candidate): Double = {
    val dealt = Damage * c.share
    val kill  = math.min(1.0, dealt / math.max(1, c.durability))
    val worth = c.value + (if (c.caster) CasterBonus else 0)
    worth * kill * (if (c.hitsAir || c.caster) 1.0 else 0.3)
  }

  def best(candidates: Seq[Candidate]): Option[Candidate] =
    candidates.map(c => c -> score(c)).filter(_._2 >= MinScore).maxByOption(_._2).map(_._1)
}
