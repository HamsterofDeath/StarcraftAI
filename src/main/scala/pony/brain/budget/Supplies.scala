package pony
package brain
package budget

case class Supplies(used: Int, total: Int) {
  def supplyUsagePercent = used.toDouble / total

  def available = total - used
}
