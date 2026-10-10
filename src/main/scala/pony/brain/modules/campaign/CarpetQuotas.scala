package pony
package brain
package modules
package campaign

/** Carpet knobs; every value is overridable with a -Dtwailight.carpet... system property. */
private[pony] object CarpetQuotas {
  private def intProp(name: String, defaultValue: Int) =
    sys.props.get(name).flatMap(_.toIntOption).filter(_ > 0).getOrElse(defaultValue)
  def tanksPerPost: Int      = intProp("twailight.carpetTanksPerPost", 2)
  def vulturesPerPost: Int   = intProp("twailight.carpetVulturesPerPost", 1)
  def goliathsPerPost: Int   = intProp("twailight.carpetGoliathsPerPost", 1)
  def tanksBeforeFlight: Int = intProp("twailight.carpetFlyTanks", 2)
  def homeGuards: Int        = intProp("twailight.carpetHomeGuards", 6)

  /** Zero spreads over every resource area on the map. */
  def maxPosts: Int = sys.props.get("twailight.carpetPosts").flatMap(_.toIntOption).filter(_ > 0).getOrElse(0)

  /** Depot boxes around carpet tanks; switch off with -Dtwailight.carpetBoxes=0. */
  def tankBoxesEnabled: Boolean = sys.props.get("twailight.carpetBoxes").forall(_ != "0")

  /** Only spend on boxes once this many minerals are unlocked. */
  def tankBoxMinMinerals: Int = intProp("twailight.carpetBoxMinMinerals", 600)
  def tankBoxMaxDepots: Int   = intProp("twailight.carpetBoxMaxDepots", 8)

  /** Open the wall once this many fighters exist and the second base stands. */
  def gateFighters: Int = intProp("twailight.carpetGateFighters", 12)
}
