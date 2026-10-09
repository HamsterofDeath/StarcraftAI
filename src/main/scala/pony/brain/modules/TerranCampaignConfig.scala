package pony
package brain
package modules

case class TerranCampaignConfig(
    minFighters: Int = 12,
    armyMinerals: Int = 1500,
    armyGas: Int = 300,
    expansionReserve: Int = 0,
    bankMinerals: Int = 1000,
    bankGas: Int = 300,
    requiredFields: Int = 2,
    minScoutFighters: Int = 6,
    minScouts: Int = 1,
    fieldUsefulFraction: Double = 0.15
) {
  require(minFighters > 0 && armyMinerals >= 0 && armyGas >= 0 && expansionReserve >= 0 &&
    bankMinerals >= 0 && bankGas >= 0 && requiredFields >= 1 && minScoutFighters >= 1 &&
    minScouts >= 1 && fieldUsefulFraction > 0.0 && fieldUsefulFraction < 1.0)
  def launch(count: Int, minerals: Int, gas: Int) =
    count >= minFighters && minerals >= armyMinerals && gas >= armyGas
  def ready(
      secondBaseOperational: Boolean,
      count: Int,
      minerals: Int,
      gas: Int,
      bankM: Int,
      bankG: Int
  ) = secondBaseOperational && launch(count, minerals, gas) &&
    bankM >= bankMinerals && bankG >= bankGas
  def holdNewArmy(
      secondBaseOperational: Boolean,
      count: Int,
      minerals: Int,
      gas: Int,
      attackLaunched: Boolean
  ) =
    secondBaseOperational && !attackLaunched && launch(count, minerals, gas)
  def expand(
      unlockedMinerals: Int,
      unlockedGas: Int,
      costMinerals: Int,
      costGas: Int,
      pending: Boolean,
      safeReachableSite: Boolean
  ) =
    !pending && safeReachableSite && unlockedMinerals >= costMinerals + expansionReserve && unlockedGas >= costGas

  /** A mineral field at or below the configured fraction remaining is no longer worth holding. */
  def fieldUseful(remainingFraction: Double) = remainingFraction > fieldUsefulFraction
}

object TerranCampaignConfig {
  def load() = {
    def number(key: String, default: Int)      = sys.props.get("twailight." + key).map(_.toInt).getOrElse(default)
    def fraction(key: String, default: Double) = sys.props.get("twailight." + key).map(_.toDouble).getOrElse(default)
    TerranCampaignConfig(
      number("minFighters", 12),
      number("armyMinerals", 1500),
      number("armyGas", 300),
      number("expansionReserve", 0),
      number("bankMinerals", 1000),
      number("bankGas", 300),
      number("requiredFields", 2),
      number("minScoutFighters", 6),
      number("minScouts", 1),
      fraction("fieldUsefulFraction", 0.15)
    )
  }
}
