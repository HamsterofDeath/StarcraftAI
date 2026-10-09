package pony
package brain

case class Supplies(used: Int, total: Int) {
  def supplyUsagePercent = used.toDouble / total

  def available = total - used
}
