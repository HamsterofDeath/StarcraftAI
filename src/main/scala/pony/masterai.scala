package pony

class ConcoctedAI(override val world: DefaultWorld) extends AIAPI with AIAPIEventDispatcher

object ConcoctedAI {
  def concoct(world: DefaultWorld) = {
    val mainAi = new MainAI
    new ConcoctedAI(world) {
      override def onTickOnApi(): Unit = {
        super.onTickOnApi()
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
    .addPlugin(new AiDebugRenderer(mainAi.universe))
    .addPlugin(new DebugHelper(mainAi))
  }
}