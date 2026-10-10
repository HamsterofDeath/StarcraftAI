package pony
package brain
package modules

import pony.Upgrades.Terran._

import scala.collection.mutable
import scala.jdk.CollectionConverters._

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
