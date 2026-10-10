package pony
package brain
package modules

/** What an enemy caster has at most, the game hiding enemy energy. */
private[pony] object EnemyEnergy {
  val Start    = 50.0
  val PerFrame = 0.033

  def estimate(framesSinceSeen: Int, max: Int): Double = math.min(max.toDouble, Start + PerFrame * framesSinceSeen)
}
