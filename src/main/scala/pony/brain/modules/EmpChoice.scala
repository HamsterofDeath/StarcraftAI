package pony
package brain
package modules

/** Where an EMP drains the most, kept free of the game; pixels. */
private[pony] object EmpChoice {

  /** A unit as the blast sees it: where it stands, its shields and energy, and whose it is. */
  final case class Blip(x: Double, y: Double, shields: Double, energy: Double, enemy: Boolean)

  /** Shields count once, energy three times: what casters lose weighs more than what shields regrow. */
  val DefaultWeights = (1.0, 3.0)

  /** Below this a blast is not worth its hundred energy. */
  val MinScore = 150.0

  /** Another Vessel leaves a spot just chosen alone this long. */
  val ClaimFrames = 48

  def parseWeights(text: String): Option[(Double, Double)] = text.split(',').map(_.trim.toDoubleOption) match {
    case Array(Some(s), Some(e)) => Some((s, e))
    case _                       => None
  }

  /** What a blast at `centre` drains of the enemy, less what it drains of our own units. */
  def score(centre: Blip, units: Seq[Blip], radius: Double, weights: (Double, Double)): Double =
    units.filter(u => math.hypot(u.x - centre.x, u.y - centre.y) <= radius).map { u =>
      val drained = u.shields * weights._1 + u.energy * weights._2
      if (u.enemy) drained else -drained
    }.sum

  def best(
      centres: Seq[Blip],
      units: Seq[Blip],
      radius: Double,
      weights: (Double, Double)
  ): Option[(Blip, Double)] =
    centres.map(c => c -> score(c, units, radius, weights)).filter(_._2 >= MinScore).maxByOption(_._2)
}
