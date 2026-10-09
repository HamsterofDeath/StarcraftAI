package pony
package brain
package modules
package strategy

/**
  * Heavy metal: a mech army of Siege Tanks and Goliaths with armory upgrades, behind the default campaign's bunkered
  * fields, joined by Battlecruisers once two bases stand. A few Marines crew the bunkers and a few Vultures lay mines.
  */
class TerranHeavyMetal(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Heavy metal"

  private def twoBases = bases.finishedBases.size >= 2

  override def suggestProducers = {
    val scale = spendScale
    IdealProducerCount(classOf[Barracks], 1)(true) ::
      IdealProducerCount(classOf[Factory], (2 + bases.finishedBases.size + scale / 2) min 6)(true) ::
      IdealProducerCount(classOf[Starport], 1 + scale / 4)(twoBases) :: Nil
  }

  override def suggestUnits = {
    val scale = spendScale
    IdealUnitRatio(classOf[Marine], 4)(true) ::
      IdealUnitRatio(classOf[Vulture], 2)(true) ::
      IdealUnitRatio(classOf[Tank], 8 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Goliath], 6 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Battlecruiser], 2 + scale / 2)(twoBases && phase.isSinceMid) :: Nil
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(true) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(true) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(true) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(unitManager.existsAndDone(classOf[MachineShop])) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(twoBases && phase.isSinceMid) :: Nil
}

final class TerranHeavyMetalPlugin
    extends StrategyPlugin("heavy-metal", "Siege Tanks and Goliaths with armory upgrades, Battlecruisers on two bases",
      autoSelectable = false)(new TerranHeavyMetal(_))
