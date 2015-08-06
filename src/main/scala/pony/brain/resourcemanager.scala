package pony
package brain

import scala.collection.mutable.ArrayBuffer

class ResourceManager(override val universe: Universe) extends HasUniverse {
  private val empty = Resources(0, 0, 0, 0, 0)

  private val locked      = ArrayBuffer.empty[LockedResources[_]]
  private val lockedSums  = LazyVal.from(calcLockedSums)
  private var myResources = empty
  def request(requests: ResourceRequests) = {
    trace(s"Incoming resource request: $requests")
    // first check if we have enough resources
    if (myUnlockedResources > requests.sum) {

    } else {

    }
  }
  private def myUnlockedResources = myResources - lockedSums.get
  def tick(): Unit = {
    myResources = universe.world.currentResources
    lockedSums.invalidate()
  }
  def currentResources = myResources
  private def calcLockedSums = locked.foldLeft(ResourceRequestSums.empty)((acc, e) => {
    acc + e
  })
}

trait ResourceApproval {
  def minerals: Int
  def gas: Int
  def supply: Int
  def success: Boolean
}

case class ResourceApprovalSuccess(minerals: Int, gas: Int, supply: Int) extends ResourceApproval {
  def success = true
}

object ResourceApprovalFail extends ResourceApproval {
  override def minerals = 0
  override def gas = 0
  override def supply = 0
  override def success = false
}

trait ResourceRequest {
  def amount: Int
}
case class MineralsRequest(amount: Int) extends ResourceRequest
case class GasRequest(amount: Int) extends ResourceRequest
case class SupplyRequest(amount: Int) extends ResourceRequest
case class ResourceRequests(requests: Seq[ResourceRequest], priority: Priority) {
  val sum = requests.foldLeft(ResourceRequestSums.empty)((acc, e) => {
    acc + e
  })
}

object ResourceRequests {
  val empty = ResourceRequests(Nil, Priority.None)
  def forUnit[T <: WrapsUnit](unitType: Class[_ <: T]) = {
    val mins = unitType.toUnitType.mineralPrice()
    val gas = unitType.toUnitType.gasPrice()
    val supply = unitType.toUnitType.supplyRequired()

    ResourceRequests(Seq(MineralsRequest(mins), GasRequest(gas), SupplyRequest(supply)), Priority.ConstructBuilding)
  }
}

case class LockedResources[T <: WrapsUnit](reqs: ResourceRequests, employer: Employer[T])

case class ResourceRequestSums(minerals: Int, gas: Int, supply: Int) {
  def +(e: LockedResources[_]): ResourceRequestSums = {
    val sum = e.reqs.sum
    copy(minerals = minerals + sum.minerals, gas = gas + sum.gas, supply = supply + sum.supply)
  }

  def +(e: ResourceRequest): ResourceRequestSums = {
    e match {
      case m: MineralsRequest => copy(minerals = minerals + m.amount)
      case g: GasRequest => copy(gas = minerals + g.amount)
      case s: SupplyRequest => copy(supply = supply + s.amount)
    }
  }
}

object ResourceRequestSums {
  val empty = ResourceRequestSums(0, 0, 0)
}


