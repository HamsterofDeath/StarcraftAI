package pony
package brain
package modules
package strategy

/**
  * A strategy the bot discovers at start through `java.util.ServiceLoader`. An implementation needs a public
  * no-argument constructor and a line naming it in `META-INF/services/pony.brain.modules.strategy.StrategyPlugin`, so a
  * jar on the classpath can add strategies without touching the bot.
  *
  * @param key            selects the strategy with `-Dtwailight.strategy=<key>`: lowercase letters, digits and dashes
  * @param autoSelectable whether the bot may pick it by score when no strategy is configured
  */
abstract class StrategyPlugin(val key: String, val description: String, val autoSelectable: Boolean)(
    factory: Universe => LongTermStrategy
) {
  require(key.matches("[a-z0-9-]+"), s"Strategy key '$key' must use lowercase letters, digits and dashes")

  def create(universe: Universe): LongTermStrategy = factory(universe)
}
