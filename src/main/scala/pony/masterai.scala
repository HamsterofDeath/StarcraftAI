package pony

class ConcoctedAI(override val world: DefaultWorld) extends AIAPI with AIAPIEventDispatcher

object ConcoctedAI {
  def concoct(world: DefaultWorld) = {
    val mainAi = new MainAI
    new ConcoctedAI(world) {
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
    .addPlugin(new ChangeSpeed)
    .addPlugin(mainAi)
    .addPlugin(new ChokePointRenderer(mainAi.universe))
    .addPlugin(new BlockedBuildingSpotsRenderer(mainAi.universe))
    .addPlugin(new UnitJobRenderer(mainAi.universe))
    .addPlugin(new UnitSecondLevelJobRenderer(mainAi.universe))
    .addPlugin(new PathDebugRenderer(mainAi.universe))
    .addPlugin(new MineralDebugRenderer(mainAi.universe))
    .addPlugin(new UnitDebugRenderer(mainAi.universe))
    .addPlugin(new StatsRenderer(mainAi.universe))
    .addPlugin(new DebugHelper(mainAi))
  }
}