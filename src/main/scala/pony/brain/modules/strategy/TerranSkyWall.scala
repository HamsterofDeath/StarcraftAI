package pony
package brain
package modules
package strategy

/** Seal the land approach with Supply Depots and fly straight to battlecruisers. */
class TerranSkyWall(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Sky wall"

  override def usesBunkerDefense = false

  override def usesWallDefense = true

  override def suggestProducers =
    IdealProducerCount(classOf[Barracks], 1)(true) ::
      IdealProducerCount(classOf[Factory], 1)(true) ::
      IdealProducerCount(classOf[Starport], 2)(true) :: Nil

  override def suggestUnits =
    IdealUnitRatio(classOf[Battlecruiser], 8)(true) ::
      IdealUnitRatio(classOf[ScienceVessel], 1)(true) :: Nil

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.ShipWeapons)(true) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(true) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(true) ::
      UpgradeToResearch(Upgrades.Terran.CruiserGun)(true) ::
      UpgradeToResearch(Upgrades.Terran.ScienceVesselEnergy)(true) ::
      UpgradeToResearch(Upgrades.Terran.Irradiate)(true) :: Nil
}

final class TerranSkyWallPlugin
    extends StrategyPlugin(
      "skywall",
      "Depot wall at the main choke, then Battlecruisers and a Science Vessel",
      autoSelectable = false
    )(new TerranSkyWall(_))
