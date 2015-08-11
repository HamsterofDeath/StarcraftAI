package pony

class ConcoctedAI(override val world: DefaultWorld) extends AIAPI with AIAPIEventDispatcher

object ConcoctedAI {
  def concoct(world: DefaultWorld) = {
    val mainAi = new MainAI
    new ConcoctedAI(world)
    .addPlugin(new WalkableRenderer)
    .addPlugin(new BuildableRenderer(true))
    .addPlugin(new UnitIdRenderer)
    .addPlugin(new MapReveal)
    .addPlugin(new FastSpeed)
    .addPlugin(mainAi)
    .addPlugin(new UnitJobRenderer(mainAi.universe))
    .addPlugin(new StatsRenderer(mainAi.universe))
    .addPlugin(new BlockedBuildingSpotsRenderer(mainAi.universe))
    .addPlugin(new DebugHelper(mainAi))
  }
}