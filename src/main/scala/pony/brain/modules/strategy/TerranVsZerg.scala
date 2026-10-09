package pony
package brain
package modules
package strategy

class TerranVsZerg(override val universe: Universe) extends LongTermStrategy with TerranDefaults {

  override def name = "M&Ms"

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.InfantryCooldown)(phase.isSinceVeryEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.MarineRange)(phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryWeapons)(phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.InfantryArmor)(phase.isSinceMid) ::
      UpgradeToResearch(Upgrades.Terran.TankSiegeMode)(phase.isSinceMid) ::
      Nil

  override def suggestUnits = {
    IdealUnitRatio(classOf[Marine], 15)(phase.isAnyTime) ::
      IdealUnitRatio(classOf[Firebat], 3)(phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Medic], 4)(phase.isSinceEarlyMid) ::
      IdealUnitRatio(classOf[Vulture], 1)(phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Tank], 3)(phase.isSinceAlmostMid) ::
      IdealUnitRatio(classOf[Goliath], 3)(phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Wraith], 1)(phase.isSinceLateMid) ::
      IdealUnitRatio(classOf[Battlecruiser], 1)(phase.isLate) ::
      IdealUnitRatio(classOf[ScienceVessel], 1)(phase.isSinceLateMid) ::
      Nil
  }

  override def determineScore = {
    if (forces.isTvZ || mapLayers.rawWalkableMap.size <= 96 * 96) 50 else 0
  }

  override def suggestProducers = {
    val myBases = bases.myMineralFields.count(_.value > 1000)
    IdealProducerCount(classOf[Barracks], myBases + 2)(phase.isAnyTime) ::
      IdealProducerCount(classOf[Barracks], (myBases / 3) max 1)(phase.isSincePostMid) ::
      IdealProducerCount(classOf[Factory], myBases)(phase.isSinceAlmostMid, hasZeroOf[Factory]) ::
      IdealProducerCount(classOf[Starport], (myBases / 2) max 1)(phase.isSinceLateMid, hasZeroOf[Starport]) ::
      Nil
  }
}

final class TerranVsZergPlugin
    extends StrategyPlugin("tvz", "Marines and Medics with Firebats versus Zerg or on small maps",
      autoSelectable = true)(new TerranVsZerg(_))
