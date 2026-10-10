package pony
package brain
package modules
package micro

/** Where an EMP drains the most, kept free of the game; pixels. */
private[pony] object EmpChoice {

  /** A unit as the blast sees it: where it stands, its shields and energy, and whose it is. */
  final case class Blip(x: Double, y: Double, shields: Double, energy: Double, enemy: Boolean)

  /**
    * Shields only. Energy counted three times (or alone) drew the blasts onto spread-out High Templar instead of the
    * Archons' 350 shields: 2 and 0 wins in 10 mixed-start games against 6 for shields only.
    */
  val DefaultWeights = (1.0, 0.0)

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
