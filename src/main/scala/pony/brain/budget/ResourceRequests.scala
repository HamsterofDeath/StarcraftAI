package pony
package brain
package budget

import pony.units.{Irrelevant, Upgrader, WrapsUnit}

import pony.brain.modules.production.UpgradePrice

case class ResourceRequests(requests: Seq[ResourceRequest], priority: Priority, whatFor: Class[?]) {
  def minerals = requests.collect { case MineralsRequest(amount) => amount }.sum

  def gas = requests.collect { case GasRequest(amount) => amount }.sum

  def supply = requests.collect { case SupplyRequest(amount) => amount }.sum

  def +(other: ResourceRequests) = {
    ResourceRequests(requests ++ other.requests, priority, whatFor)
  }

  val sum = requests.foldLeft(ResourceRequestSum.empty)((acc, e) => {
    acc + e
  })
}

object ResourceRequests {
  val empty = ResourceRequests(Nil, Priority.None, classOf[Irrelevant])

  def forUpgrade(
      upgrader: Upgrader,
      price: UpgradePrice,
      priority: Priority = Priority.Upgrades
  ) = {
    val upgrade = price.forUpgrade
    val mins    = price.nextMineralPrice
    val gas     = price.nextGasPrice
    ResourceRequests(Seq(MineralsRequest(mins), GasRequest(gas)), priority, upgrader.getClass)

  }

  def forUnit[T <: WrapsUnit](
      race: SCRace,
      unspecificType: Class[? <: T],
      priority: Priority = Priority.Default
  ) = {
    val unitType = race.specialize(unspecificType)
    val mins     = unitType.toUnitType.mineralPrice()
    val gas      = unitType.toUnitType.gasPrice()
    val supply   = unitType.toUnitType.supplyRequired()

    val requestDetails = Seq(MineralsRequest(mins), GasRequest(gas), SupplyRequest(supply))
    ResourceRequests(requestDetails, priority, unitType)
  }
}
