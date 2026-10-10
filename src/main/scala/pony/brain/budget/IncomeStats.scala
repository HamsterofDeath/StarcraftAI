package pony
package brain
package budget

case class IncomeStats(minerals: Int, gas: Int, frames: Int) {
  def mineralsPerMinute = 60 * mineralsPerSecond

  def mineralsPerSecond = 24 * minerals.toDouble / ResourceManager.frameSizeForStats

  def gasPerMinute = 60 * gasPerSecond

  def gasPerSecond = 24 * gas.toDouble / ResourceManager.frameSizeForStats
}
