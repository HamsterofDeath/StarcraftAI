package pony
package brain
package modules
package strategy

/**
  * Chooses the strategy of one game: the configured plugin, or while none is configured the best-scoring
  * auto-selectable plugin (idle until the first evaluation). An unknown configured key stops the bot at start.
  */
class StrategySelector(override val universe: Universe, registry: StrategyRegistry, configuredKey: Option[String])
    extends HasUniverse {

  def this(universe: Universe) =
    this(universe, StrategyRegistry.discover(), sys.props.get(StrategySelector.Property))

  private val configured = configuredKey.filterNot(_ == StrategySelector.Auto).map { key =>
    registry.find(key).getOrElse(throw new IllegalArgumentException(
      s"Unknown strategy '$key'; known: ${(StrategySelector.Auto +: registry.keys).mkString(", ")}"
    )).create(universe)
  }

  private val candidates =
    if (configured.isDefined) Vector.empty else registry.plugins.filter(_.autoSelectable).map(_.create(universe))

  private var best: LongTermStrategy = new IdleAround(universe)

  def current: LongTermStrategy = configured.getOrElse(best)

  def available: Vector[StrategyPlugin] = registry.plugins

  def tick(): Unit = {
    if (candidates.nonEmpty) ifNth(Primes.prime251) {
      val next = candidates.maxBy(_.determineScore)
      if (next ne best) NativeMatchEvidence.trace("strategy-chosen", s"name=${next.name} score=${next.determineScore}")
      best = next
    }
  }
}

object StrategySelector {
  val Property = "twailight.strategy"

  /** The key that leaves the choice to the bot. */
  val Auto = "default"
}
