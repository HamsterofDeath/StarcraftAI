package pony

class ConcoctedAI(override val world: DefaultWorld) extends AIAPI

object ConcoctedAI {
  def concoct(world: DefaultWorld) = {
    val mainAi = new MainAI
    new ConcoctedAI(world)
    .addPlugin(new WalkableRenderer)
    .addPlugin(new UnitIdRenderer)
    .addPlugin(new MapReveal)
    .addPlugin(mainAi)
    .addPlugin(new UnitJobRenderer(mainAi.brain.universe))
  }
}