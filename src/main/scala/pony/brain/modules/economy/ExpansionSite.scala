package pony
package brain
package modules
package economy

/**
  * Orders candidate mineral fields for a new base: fields our SCVs can walk to from our start first, then fields away
  * from the enemy buildings we know, then fields on our half of the map (closer to our start than to every possible
  * enemy start), then the closest to our main base, then fields with a defense line. Pure, so the choice can be tested
  * without a map; positions are tiles.
  */
private[pony] object ExpansionSite {

  /**
    * `reachable` is false for an island: in game 1 vs Protoss on 9497320 on ICCup Lost_Temple the main's depot flew to
    * the island in the corner, which counted as our half; no SCV could walk there, the ones it trained stood idle, and
    * the economy stalled at 146 supply with 66 idle SCVs for forty game minutes.
    *
    * `nearEnemy` is true within EnemyTiles of a known enemy building: in game 9 vs Terran on 74b88f4 on Lost_Temple the
    * east main's next depot was built toward the north main's natural (93,2), 35 tiles from our main as the crow flies
    * and 36 from the enemy's; both depots that flew there were lost, and our economy with them.
    */
  final case class Candidate(
      id: Int,
      x: Int,
      y: Int,
      defenseLine: Boolean,
      reachable: Boolean = true,
      nearEnemy: Boolean = false
  )

  /** How near a known enemy building a field counts as theirs. */
  val EnemyTiles = 40

  private def distance(ax: Int, ay: Int, bx: Int, by: Int) = math.hypot(ax - bx, ay - by)

  def rank(
      candidates: Seq[Candidate],
      main: (Int, Int),
      ourStart: (Int, Int),
      enemyStarts: Seq[(Int, Int)]
  ): Seq[Candidate] = candidates.sortBy { c =>
    val ours   = distance(c.x, c.y, ourStart._1, ourStart._2)
    val theirs = enemyStarts.map(s => distance(c.x, c.y, s._1, s._2)).minOption.getOrElse(Double.MaxValue)
    (
      if (c.reachable) 0 else 1,
      if (c.nearEnemy) 1 else 0,
      if (ours < theirs) 0 else 1,
      distance(c.x, c.y, main._1, main._2),
      if (c.defenseLine) 0 else 1,
      c.id
    )
  }
}
