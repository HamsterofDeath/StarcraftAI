package pony

class ConcoctedAI(override val world: DefaultWorld) extends AIAPI with AIAPIEventDispatcher

object ConcoctedAI {
  def concoct(world: DefaultWorld) = {
    val mainAi = new MainAI
    new ConcoctedAI(world) {
      private var lastTickNanos = System.nanoTime()
      override def onTickOnApi(): Unit = {
        val before = System.nanoTime()
        super.onTickOnApi()
        val after = System.nanoTime()
        val nanoDiff = after - before
        debug(s"AI took ${nanoDiff.toDouble / 1000 / 1000} ms for calculations")
      }
    }
    .addPlugin(new WalkableRenderer)
    .addPlugin(new BuildableRenderer(true))
    .addPlugin(new UnitIdRenderer)
    .addPlugin(new MapReveal)
    .addPlugin(new FastSpeed)
    .addPlugin(mainAi)
    .addPlugin(new UnitJobRenderer(mainAi.universe))
    .addPlugin(new StatsRenderer(mainAi.universe))
    .addPlugin(new BlockedBuildingSpotsRenderer(mainAi.universe))
    .addPlugin(new ChokePointRenderer(mainAi.universe))
    .addPlugin(new MineralDebugRenderer(mainAi.universe))
    .addPlugin(new DebugHelper(mainAi))
  }
}