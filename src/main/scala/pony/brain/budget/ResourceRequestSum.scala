package pony
package brain
package budget

case class ResourceRequestSum(minerals: Int, gas: Int, supply: Int) {
  def canCoverCost(other: ResourceRequestSum) =
    (minerals >= other.minerals || other.minerals == 0) &&
      (gas >= other.gas || other.gas == 0) &&
      (supply >= other.supply || other.supply == 0)

  def +(other: ResourceRequestSum) = ResourceRequestSum(
    minerals + other.minerals,
    gas + other.gas,
    supply + other.supply
  )

  def mineralGasSum: Int = minerals + gas

  def equalValue(proof: ResourceApprovalSuccess) = minerals == proof.minerals && gas == proof.gas &&
    supply == proof.supply

  def +(e: LockedResources[?]): ResourceRequestSum = {
    val sum = e.reqs.sum
    copy(minerals = minerals + sum.minerals, gas = gas + sum.gas, supply = supply + sum.supply)
  }

  def +(e: ResourceRequest): ResourceRequestSum = {
    e match {
      case m: MineralsRequest => copy(minerals = minerals + m.amount)
      case g: GasRequest      => copy(gas = gas + g.amount)
      case s: SupplyRequest   => copy(supply = supply + s.amount)
    }
  }
}

object ResourceRequestSum {
  val empty                                      = ResourceRequestSum(0, 0, 0)
  implicit val ord: Ordering[ResourceRequestSum] = Ordering.by[ResourceRequestSum, Int](_.mineralGasSum)
}
