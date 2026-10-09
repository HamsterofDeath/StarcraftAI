package pony
package brain
package modules
package strategy

class TerranVsProtoss(override val universe: Universe) extends LongTermStrategy with TerranDefaults {
  override def determineScore = {
    if (forces.isTvP) 50 else 0
  }

  override def name = "TvP"

  override def suggestProducers = {
    val myBases = bases.myMineralFields.count(_.remainingPercentage > 0.25)

    IdealProducerCount(classOf[Barracks], (myBases / 2) max 1)(phase.isAnyTime) ::
      IdealProducerCount(classOf[Factory], 2 + myBases)(phase.isAnyTime) ::
      IdealProducerCount(classOf[Starport], (myBases / 2) max 1)(phase.isSincePostMid) ::
      Nil
  }

  override def suggestUnits = {
    IdealUnitRatio(classOf[Marine], 3)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Medic], 1)(phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Ghost], 2)(phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Vulture], 6)(phase.isAnyTime) ::
      IdealUnitRatio(classOf[Tank], 10)(phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Goliath], 6)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 2)(phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Dropship], 1)(phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Wraith], 2)(phase.isSinceLateMid) ::
      Nil
  }

  override def suggestUpgrades: Seq[UpgradeToResearch] =
    UpgradeToResearch(Upgrades.Terran.SpiderMines)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(phase.isSinceAlmostMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.EMP)(phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(phase.isSinceLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.MarineRange)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostStop)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostCloak)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostEnergy)(phase.isSinceVeryLateMid) ::
      UpgradeToResearch(Upgrades.Terran.GhostVisiblityRange)(phase.isSinceVeryLateMid) ::
      Nil
}

final class TerranVsProtossPlugin
    extends StrategyPlugin(
      "tvp",
      "Mech-heavy Terran versus Protoss, with late Ghosts, Wraiths and Science Vessels",
      autoSelectable = true
    )(new TerranVsProtoss(_))
