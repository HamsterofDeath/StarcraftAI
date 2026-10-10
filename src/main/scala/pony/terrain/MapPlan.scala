package pony
package terrain

/**
  * The maps of a warm session, `-Dtwailight.mapPlan=maps/e2e/run/001-a.scm,maps/e2e/run/002-b.scm`: BWAPI plays them
  * in order in one StarCraft process (a wildcard map with sequential iteration) and the bot leaves after the last one.
  * Without a plan the bot plays one game.
  */
object MapPlan {
  val maps: Vector[String] =
    sys.props.get("twailight.mapPlan").toVector.flatMap(_.split(',').map(_.trim).filter(_.nonEmpty))

  def isSession: Boolean = maps.size > 1

  def isLast(number: Int): Boolean = number >= maps.size
}
