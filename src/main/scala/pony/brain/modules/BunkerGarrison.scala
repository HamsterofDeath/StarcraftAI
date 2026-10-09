package pony
package brain
package modules

/** Boarding intentions remain reserved; acceptance still requires four actual native cargo units. */
private[pony] class BunkerGarrison {
  private var assigned = Map.empty[Int, Vector[Int]]
  def update(bunkers: Seq[(Int, MapTilePosition, Set[Int])], marines: Seq[(Int, MapTilePosition)]): Unit = {
    val alive     = marines.map(_._1).toSet
    val bunkerIds = bunkers.map(_._1).toSet
    assigned = assigned.filter(e => bunkerIds(e._1)).map { case (id, ids) => id -> ids.filter(alive).take(4) }
    val loaded = bunkers.flatMap(_._3).toSet
    assigned = assigned.map { case (id, ids) => id -> ids.filterNot(loaded) }
    bunkers.sortBy(_._1).foreach { case (id, tile, cargo) =>
      val kept = (cargo.toVector.sorted ++ assigned.getOrElse(id, Vector.empty)).distinct.take(4)
      val busy = assigned.values.flatten.toSet ++ loaded ++ kept
      val fill = marines.filterNot(m => busy(m._1)).sortBy(m => (m._2.distanceSquaredTo(tile), m._1))
        .take(4 - kept.size).map(_._1)
      assigned += id -> (kept ++ fill)
    }
  }
  def target(marine: Int) = assigned.collectFirst { case (id, ids) if ids.contains(marine) => id }
  def reserved            = assigned.values.flatten.toSet
}
