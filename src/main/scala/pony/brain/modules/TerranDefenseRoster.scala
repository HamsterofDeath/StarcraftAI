package pony
package brain
package modules

/** Stable guards stay with each distinct landed field; casualties are replaced locally. */
private[pony] class TerranDefenseRoster(perField: Int) {
  private var members                                                               = Map.empty[Int, Vector[Int]]
  private var fields                                                                = Map.empty[Int, MapTilePosition]
  def update(currentFields: Seq[DefenseField], fighters: Seq[DefenseFighter]): Unit = {
    fields = currentFields.map(f => f.id -> f.rally).toMap
    val mobile = fighters.filterNot(_.garrisonReserved)
    val live   = mobile.map(_.id).toSet
    members = members.filter(p => fields.contains(p._1)).map { case (field, ids) => field -> ids.filter(live) }
    var used = members.values.flatten.toSet
    currentFields.sortBy(_.id).foreach { field =>
      val kept         = members.getOrElse(field.id, Vector.empty)
      val replacements = mobile.filterNot(f => used(f.id) || f.campaignAssigned)
        .sortBy(f => (f.tile.distanceSquaredTo(field.rally), f.id)).take(perField - kept.size).map(_.id)
      members += field.id -> (kept ++ replacements)
      used ++= replacements
    }
  }
  def reserved          = members.values.flatten.toSet
  def rallyFor(id: Int) = members.find(_._2.contains(id)).flatMap(p => fields.get(p._1))
}
