package pony

class ConcoctedAI(override val world: DefaultWorld) extends AIAPI

object ConcoctedAI {
  def concoct(world: DefaultWorld) = new ConcoctedAI(world)
                                     .addPlugin(new WalkableRenderer)
                                     .addPlugin(new MapReveal)
                                     .addPlugin(new MainAI)
}