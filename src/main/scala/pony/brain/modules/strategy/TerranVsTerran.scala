package pony
package brain
package modules
package strategy

class TerranVsTerran(override val universe: Universe) extends LongTermStrategy with TerranDefaults {
  override def name = "TvT"

  override def determineScore = {
    val ok = forces.isTvT
    if (ok) 50 else 0
  }

  override def suggestUnits = {
    IdealUnitRatio(classOf[Marine], 3)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Medic], 1)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Ghost], 3)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Vulture], 3)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Tank], 3)(phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Dropship], 3)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[Goliath], 5)(phase.isSincePostMid) ::
      IdealUnitRatio(classOf[Wraith], 20)(phase.isSinceMid) ::
      IdealUnitRatio(classOf[ScienceVessel], 5)(phase.isSincePostMid) ::
      Nil
  }

  override def suggestUpgrades =
    UpgradeToResearch(Upgrades.Terran.WraithCloak)(phase.isSinceEarlyMid) ::
      UpgradeToResearch(Upgrades.Terran.WraithEnergy)(phase.isSinceMid) ::
      Nil

  override def suggestProducers = {
    val myBases = bases.myMineralFields.count(_.value > 1000)

    IdealProducerCount(classOf[Barracks], myBases)(phase.isAnyTime) ::
      IdealProducerCount(classOf[Factory], myBases)(phase.isAnyTime) ::
      IdealProducerCount(classOf[Starport], myBases * 3)(phase.isAnyTime) ::
      Nil
  }
}

final class TerranVsTerranPlugin
    extends StrategyPlugin("tvt", "Wraith-heavy Terran versus Terran with Ghosts, Dropships and Science Vessels",
      autoSelectable = true)(new TerranVsTerran(_))
