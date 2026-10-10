package pony
package brain
package modules
package strategy

import pony.brain.modules.production.{IdealProducerCount, IdealUnitRatio}
import pony.tech.Upgrades
import pony.units.{Barracks, Firebat, Marine, Medic}

/** Maximum-offense infantry: barracks only, every infantry upgrade, no bunkers at home. */
class TerranInfantryPush(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Infantry push"

  override def usesBunkerDefense = false

  override def suggestProducers =
    IdealProducerCount(classOf[Barracks], 3)(true) :: Nil

  override def suggestUnits =
    IdealUnitRatio(classOf[Marine], 12)(true) ::
      IdealUnitRatio(classOf[Firebat], 3)(time.minutes >= 3) ::
      IdealUnitRatio(classOf[Medic], 3)(time.minutes >= 3) :: Nil

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(true) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(true) ::
      UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(true) ::
      UpgradeToResearch(Upgrades.Terran.MarineRange)(true) ::
      UpgradeToResearch(Upgrades.Terran.MedicEnergy)(true) ::
      UpgradeToResearch(Upgrades.Terran.MedicHeal)(true) ::
      UpgradeToResearch(Upgrades.Terran.MedicFlare)(true) :: Nil
}

final class TerranInfantryPushPlugin
    extends StrategyPlugin(
      "infantry",
      "Three barracks of upgraded Marines, Firebats and Medics; no bunkers",
      autoSelectable = false
    )(new TerranInfantryPush(_))
