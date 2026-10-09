package pony
package brain
package modules
package strategy

/** Builds and researches nothing; the starting point of every game and the strategy of micro scenarios. */
class IdleAround(override val universe: Universe) extends LongTermStrategy {
  override def name = "Idle"

  override def buildAntiCloakNow = false

  override def determineScore = -1

  override def suggestProducers = Nil

  override def suggestUnits = Nil

  override def suggestUpgrades = Nil

  override def suggestNextExpansion = None
}

final class IdleAroundPlugin
    extends StrategyPlugin(
      "idle",
      "Builds nothing; units only follow their default behaviours (micro scenarios)",
      autoSelectable = false
    )(new IdleAround(_))
