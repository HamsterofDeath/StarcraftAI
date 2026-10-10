package pony
package brain
package modules
package economy

/**
  * Orders candidate mineral fields for a new base: fields on our half of the map (closer to our start than to every
  * possible enemy start) first, then the closest to our main base, then fields with a defense line. Pure, so the choice
  * can be tested without a map; positions are tiles.
  */
private[pony] object ExpansionSite {

  final case class Candidate(id: Int, x: Int, y: Int, defenseLine: Boolean)

  private def distance(ax: Int, ay: Int, bx: Int, by: Int) = math.hypot(ax - bx, ay - by)

  def rank(
      candidates: Seq[Candidate],
      main: (Int, Int),
      ourStart: (Int, Int),
      enemyStarts: Seq[(Int, Int)]
  ): Seq[Candidate] = candidates.sortBy { c =>
    val ours   = distance(c.x, c.y, ourStart._1, ourStart._2)
    val theirs = enemyStarts.map(s => distance(c.x, c.y, s._1, s._2)).minOption.getOrElse(Double.MaxValue)
    (if (ours < theirs) 0 else 1, distance(c.x, c.y, main._1, main._2), if (c.defenseLine) 0 else 1, c.id)
  }
}
