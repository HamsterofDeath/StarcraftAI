package pony
package brain
package modules
package strategy

import pony.brain.modules.production.{IdealProducerCount, IdealUnitRatio}
import pony.brain.modules.wall.WallWithDepots
import pony.tech.Upgrades
import pony.units.{Barracks, Factory, Goliath, Marine, Tank, Vulture}

/** Depot wall, tanks behind it, then flying factories and a mine-backed tank carpet. */
class TerranCarpet(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Carpet"

  private def wallRefused = universe.pluginByType[WallWithDepots].refused

  // An unsealable choke falls back to the default bunker defense so raids find no naked base.
  override def usesBunkerDefense = wallRefused

  override def usesWallDefense = true

  override def usesCampaignLaunch = false

  override def usesCarpet = true

  override def suggestProducers = {
    val scale = spendScale
    IdealProducerCount(classOf[Barracks], if (wallRefused) 1 else 0)(true) ::
      IdealProducerCount(classOf[Factory], ((bases.finishedBases.size max 1) + scale) min 6)(true) :: Nil
  }

  override def suggestUnits = {
    val scale = spendScale
    IdealUnitRatio(classOf[Marine], if (wallRefused) 8 else 0)(true) ::
      IdealUnitRatio(classOf[Tank], 8 + scale * 3)(true) ::
      IdealUnitRatio(classOf[Vulture], 6 + scale * 2)(true) ::
      IdealUnitRatio(classOf[Goliath], 2 + scale)(true) :: Nil
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(true) ::
      UpgradeToResearch(Upgrades.Terran.SpiderMines)(true) ::
      UpgradeToResearch(Upgrades.Terran.VultureSpeed)(true) ::
      UpgradeToResearch(Upgrades.Terran.VehicleWeapons)(true) ::
      UpgradeToResearch(Upgrades.Terran.VehicleArmor)(true) ::
      UpgradeToResearch(Upgrades.Terran.GoliathRange)(true) :: Nil
}

final class TerranCarpetPlugin
    extends StrategyPlugin(
      "carpet",
      "Depot wall, then flying factories and boxed tank posts with mines map-wide",
      autoSelectable = false
    )(new TerranCarpet(_))
