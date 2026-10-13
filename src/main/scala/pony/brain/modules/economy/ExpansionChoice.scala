package pony
package brain
package modules
package economy

/**
  * How the expander closes a deficit of healthy mineral fields. A new command center is preferred over moving one that
  * still mines: only a depot freshly built at home (which must fly to its own field) or a depot on an exhausted field
  * flies, and the exhausted one only when no new depot is under way or affordable.
  */
private[pony] object ExpansionChoice {
  sealed trait Choice

  /** Nothing to do: the fields are held, or a new depot is already coming. */
  case object Hold extends Choice

  final case class Move(depotId: Int) extends Choice

  case object BuildNew extends Choice

  case object WaitForMinerals extends Choice

  /**
    * Healthy fields to hold: at least `required`, and one more than held once every held field is fully staffed,
    * because then only another field grows the income; never more than `max`.
    */
  def wantedFields(required: Int, held: Int, allSaturated: Boolean, max: Int): Int =
    math.min(max, if (held >= required && allSaturated) held + 1 else required)

  /**
    * The fresh depots among those sharing a field, by id with whether each stands at its field: those that do not,
    * if any; a depot standing at its field flies only when every sharer does.
    */
  def fliers(sharing: Seq[(Int, Boolean)]): Seq[Int] = {
    val away = sharing.collect { case (id, false) => id }
    if (away.nonEmpty) away else sharing.map(_._1)
  }

  /**
    * Idle SCVs that count as full crews: in game 5 vs Protoss on 09c3bb2 a dozen stood idle for 18 minutes while the
    * expander waited for its fields to fill up.
    */
  val IdleSurplus = 8

  /**
    * @param deficit     healthy fields missing
    * @param fresh       depots built at home that have not flown to a field of their own yet
    * @param exhausted   depots whose field is mined out
    * @param newUnderWay a new depot is requested or under construction
    * @param affordable  the unlocked bank pays for a new depot now
    */
  def decide(deficit: Int, fresh: Seq[Int], exhausted: Seq[Int], newUnderWay: Boolean, affordable: Boolean): Choice =
    if (deficit <= 0) Hold
    else if (fresh.nonEmpty) Move(fresh.min)
    else if (newUnderWay) Hold
    else if (affordable) BuildNew
    else exhausted.minOption.map(Move(_)).getOrElse(WaitForMinerals)
}
