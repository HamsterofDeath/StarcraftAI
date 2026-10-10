package pony

import pony.brain.budget.{ResourceRequestSum, Supplies}

case class Resources(minerals: Int, gas: Int, supply: Supplies) {
  val asSum = ResourceRequestSum(minerals, gas, supply.available)

  def moreGasThanMinerals = minerals < gas

  def >(min: Int, gas: Int, supply: Int) = {
    minerals >= min && this.gas >= gas && this.supplyRemaining >= supply
  }

  def supplyRemaining = supply.available

  def moreMineralsThanGas = minerals > gas * 1.5

  def supplyTotal = supply.total

  def -(sums: ResourceRequestSum) = {
    val supplyUpdated = supply.copy(supplyUsed + sums.supply)
    copy(minerals - sums.minerals, gas - sums.gas, supply = supplyUpdated)
  }

  def supplyUsed = supply.used

  def supplyUsagePercent = supply.supplyUsagePercent
}
