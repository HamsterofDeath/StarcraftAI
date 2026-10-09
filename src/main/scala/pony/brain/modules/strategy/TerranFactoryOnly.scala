package pony
package brain
package modules
package strategy

/** No infantry at all: everything comes from the factory, with vehicle upgrades. */
class TerranFactoryOnly(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Factory only"

  override def usesBunkerDefense = false

  override def suggestProducers =
    IdealProducerCount(classOf[Factory], 4)(true) :: Nil

  override def suggestUnits =
    IdealUnitRatio(classOf[Vulture], 4)(true) ::
      IdealUnitRatio(classOf[Tank], 8)(true) ::
      IdealUnitRatio(classOf[Goliath], 4)(true) :: Nil

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(true) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(true) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(true) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(true) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(true) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(true) :: Nil
}

final class TerranFactoryOnlyPlugin
    extends StrategyPlugin(
      "factory",
      "Four factories of Vultures, Tanks and Goliaths; no infantry, no bunkers",
      autoSelectable = false
    )(new TerranFactoryOnly(_))
