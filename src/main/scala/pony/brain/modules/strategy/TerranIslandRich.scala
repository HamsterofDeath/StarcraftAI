package pony
package brain
package modules
package strategy

/** Air control with Battlecruisers and Science Vessels once the bases are rich. */
class TerranIslandRich(universe: Universe) extends TerranIsland(universe) with TerranDefaults {

  override def name = "Heavy air"

  override def suggestUnits: List[IdealUnitRatio[? <: Mobile]] = {
    IdealUnitRatio(classOf[ScienceVessel], 3)(phase.isAnyTime) ::
      IdealUnitRatio(classOf[Battlecruiser], 10)(phase.isAnyTime) ::
      super.suggestUnits.toList
  }

  override def determineScore: Int = {
    super.determineScore + bases.rich.ifElse(10, -10)
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.CruiserGun)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.CruiserEnergy)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.EMP)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ShipWeapons)(phase.isAnyTime) ::
      UpgradeToResearch(Upgrades.Terran.ShipArmor)(phase.isAnyTime) ::
      super.suggestUpgrades.toList
}

final class TerranIslandRichPlugin
    extends StrategyPlugin(
      "heavy-air",
      "Air control with Battlecruisers and Science Vessels for rich island starts",
      autoSelectable = true
    )(new TerranIslandRich(_))
