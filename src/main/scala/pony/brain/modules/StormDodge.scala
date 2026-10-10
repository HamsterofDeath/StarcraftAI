package pony
package brain
package modules

/** Where to flee a storm, kept free of the game; pixels. */
private[pony] object StormDodge {

  /** A storm covers about three tiles square; this close to its centre a unit is in it or about to be. */
  val Reach = 80.0

  /** How far a fleeing unit goes from the storm's centre. */
  val Escape = 128.0

  def escape(at: (Double, Double), storms: Seq[(Double, Double)], underStorm: Boolean): Option[(Double, Double)] = {
    val near = storms.filter(s => math.hypot(at._1 - s._1, at._2 - s._2) < Reach)
    if (near.isEmpty && !underStorm) None
    else {
      val (sx, sy) =
        if (near.nonEmpty) (near.map(_._1).sum / near.size, near.map(_._2).sum / near.size)
        else storms.minBy(s => math.hypot(at._1 - s._1, at._2 - s._2))
      val (dx, dy) = (at._1 - sx, at._2 - sy)
      val length   = math.hypot(dx, dy)
      val (ux, uy) = if (length < 1) (1.0, 0.0) else (dx / length, dy / length)
      Some((sx + ux * Escape, sy + uy * Escape))
    }
  }
}
