package pony
package brain
package modules
package strategy

/** Air control for island starts: starports scale with rich bases, Wraiths lead. */
class TerranIsland(override val universe: Universe) extends LongTermStrategy with TerranDefaults {

  override def name = "Air control"

  override def suggestUnits = {
    IdealUnitRatio(classOf[Marine], 3)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Medic], 1)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Ghost], 1)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Vulture], 1)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Tank], 1)(phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Dropship], 1)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Goliath], 1)(phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Wraith], 8)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Battlecruiser], 3)(phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 2)(phase.isSincePostMid) ::
      Nil
  }

  override def determineScore: Int = {
    bases.mainBase.map { mb =>
      mapLayers.isOnIsland(mb.mainBuilding.tilePosition)
        .ifElse(100, 0)
    }.getOrElse(0)
  }

  override def suggestProducers = {
    val myBases = bases.myMineralFields.count(_.value > 1000)

    IdealProducerCount(classOf[Barracks], myBases)(phase.isAnyTime) ::
      IdealProducerCount(classOf[Factory], myBases)(phase.isAnyTime) ::
      IdealProducerCount(classOf[Starport], myBases * 3)(phase.isAnyTime) ::
      Nil
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.WraithCloak)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.WraithEnergy)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.EMP)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostCloak)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicFlare)(phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicHeal)(phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.MedicEnergy)(phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostEnergy)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(phase.isSinceVeryLateMid) ::
      Nil
}

final class TerranIslandPlugin
    extends StrategyPlugin(
      "air-control",
      "Wraith-led air control, chosen automatically on island starts",
      autoSelectable = true
    )(new TerranIsland(_))
