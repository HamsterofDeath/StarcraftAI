package pony
package brain

import scala.collection.mutable.ArrayBuffer

case class IncomeStats(minerals: Int, gas: Int, frames: Int)

class ResourceManager(override val universe: Universe) extends HasUniverse {

  private val resourceHistory         = ArrayBuffer.empty[MinsGas]
  private val frameSizeForStats       = 600
  // should be 10 "seconds" i guess?
  private val empty                   = Resources(0, 0, Supplies(0, 0))
  private val locked                  = ArrayBuffer.empty[LockedResources[_]]
  private val lockedSums              = LazyVal.from(calcLockedSums)
  private val failedToProvideThisTick = ArrayBuffer.empty[ResourceRequests]
  private var myResources             = empty
  private var failedToProvideLastTick = Vector.empty[ResourceRequests]
  def failedToProvide = failedToProvideLastTick

  def stats = {
    val start = resourceHistory.head
    val now = resourceHistory.last
    val minPlus = start.mins - start.mins
    val gasPlus = start.gas - start.gas
    IncomeStats(minPlus, gasPlus, resourceHistory.size)
  }

  def detailledLocks = locked.toSeq

  def unlock_!(proofForFunding: ResourceApprovalSuccess): Unit = {
    locked.removeFirstMatch(_.reqs.sum.equalValue(proofForFunding))
    trace(s"Unlocked $proofForFunding")
    lockedSums.invalidate()
  }

  def request[T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T]) = {
    trace(s"Incoming resource request: $requests")
    // first check if we have enough resources
    if (myUnlockedResources > requests.sum) {
      lock_!(requests, employer)
      ResourceApprovalSuccess(requests.sum)
    } else {
      // TODO unlock resources with lesser priority, biggest first
      failedToProvideThisTick += requests
      ResourceApprovalFail
    }
  }

  private def lock_![T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T]): Unit = {
    val newLock = LockedResources(requests, employer)
    trace(s"Locked $newLock")
    locked += newLock
    lockedSums.invalidate()
  }

  private def myUnlockedResources = myResources - lockedResources

  def lockedResources = lockedSums.get

  def tick(): Unit = {
    failedToProvideLastTick = failedToProvideThisTick.toVector
    failedToProvideThisTick.clear()

    myResources = universe.world.currentResources
    lockedSums.invalidate()

    resourceHistory += MinsGas(resources.gatheredMinerals, resources.gatheredGas)
    if (resourceHistory.size == frameSizeForStats + 1) {
      resourceHistory.remove(0)
    }
  }

  def gatheredMinerals = nativeGame.self().gatheredMinerals()
  def gatheredGas = nativeGame.self().gatheredGas()
  def currentResources = myResources
  def supplies = myUnlockedResources.supply
  def plannedSuppliesToAdd = {
    unitManager
    .selectJobs((e: ConstructBuilding[WorkerUnit, Building]) => e.typeOfBuilding == unitManager.race.supplyClass)
  }
  private def calcLockedSums = locked.foldLeft(ResourceRequestSums.empty)((acc, e) => {
    acc + e
  })
}

trait ResourceApproval {
  def minerals: Int
  def gas: Int
  def supply: Int
  def success: Boolean
  def isFunded = success
  def assumeSuccessful = {
    assert(success)
    this.asInstanceOf[ResourceApprovalSuccess]
  }
}

case class Supplies(used: Int, total: Int) {
  def supplyUsagePercent = used.toDouble / total

  def available = total - used
}

case class ResourceApprovalSuccess(minerals: Int, gas: Int, supply: Int) extends ResourceApproval {
  def success = true
}

object ResourceApprovalSuccess {
  def apply(sums: ResourceRequestSums): ResourceApprovalSuccess = ResourceApprovalSuccess(sums.minerals, sums.gas,
    sums.supply)
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
case class ResourceRequests(requests: Seq[ResourceRequest], priority: Priority, whatFor: Class[_ <: WrapsUnit]) {
  val sum = requests.foldLeft(ResourceRequestSums.empty)((acc, e) => {
    acc + e
  })
}

object ResourceRequests {
  val empty = ResourceRequests(Nil, Priority.None, classOf[Irrelevant])
  def forUnit[T <: WrapsUnit](unitType: Class[_ <: T], priority: Priority = Priority.Default) = {
    val mins = unitType.toUnitType.mineralPrice()
    val gas = unitType.toUnitType.gasPrice()
    val supply = unitType.toUnitType.supplyRequired()

    ResourceRequests(Seq(MineralsRequest(mins), GasRequest(gas), SupplyRequest(supply)), priority, unitType)
  }
}

case class LockedResources[T <: WrapsUnit](reqs: ResourceRequests, employer: Employer[T])

case class ResourceRequestSums(minerals: Int, gas: Int, supply: Int) {
  def equalValue(proof: ResourceApprovalSuccess) = minerals == proof.minerals && gas == proof.gas &&
                                                   supply == proof.supply

  def +(e: LockedResources[_]): ResourceRequestSums = {
    val sum = e.reqs.sum
    copy(minerals = minerals + sum.minerals, gas = gas + sum.gas, supply = supply + sum.supply)
  }

  def +(e: ResourceRequest): ResourceRequestSums = {
    e match {
      case m: MineralsRequest => copy(minerals = minerals + m.amount)
      case g: GasRequest => copy(gas = gas + g.amount)
      case s: SupplyRequest => copy(supply = supply + s.amount)
    }
  }
}
case class MinsGas(mins: Int, gas: Int)

object ResourceRequestSums {
  val empty = ResourceRequestSums(0, 0, 0)
}


