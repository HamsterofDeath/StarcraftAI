package pony
package brain

import pony.brain.modules.UpgradePrice

import scala.collection.mutable.ArrayBuffer

case class IncomeStats(minerals: Int, gas: Int, frames: Int) {
  def mineralsPerMinute = 60 * mineralsPerSecond
  def mineralsPerSecond = 24 * minerals.toDouble / ResourceManager.frameSizeForStats
  def gasPerMinute = 60 * gasPerSecond
  def gasPerSecond = 24 * gas.toDouble / ResourceManager.frameSizeForStats
}

object ResourceManager {
  val frameSizeForStats = 600

}

class ResourceManager(override val universe: Universe) extends HasUniverse {

  private val resourceHistory         = ArrayBuffer.empty[MinsGas]
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
    val minPlus = now.mins - start.mins
    val gasPlus = now.gas - start.gas
    IncomeStats(minPlus, gasPlus, resourceHistory.size)
  }

  def detailledLocks = locked.toSeq

  def unlock_!(proofForFunding: ResourceApprovalSuccess): Unit = {
    locked.removeFirstMatch(_.reqs.sum.equalValue(proofForFunding))
    trace(s"Unlocked $proofForFunding")
    lockedSums.invalidate()
  }

  def request[T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T], lock: Boolean = true) = {
    trace(s"Incoming resource request: $requests")
    // first check if we have enough resources
    if (myUnlockedResources > requests.sum) {
      if (lock) lock_!(requests, employer)
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
    if (resourceHistory.size == ResourceManager.frameSizeForStats + 1) {
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
  def ifSuccess[T](then: ResourceApprovalSuccess => T): T
}

case class Supplies(used: Int, total: Int) {
  def supplyUsagePercent = used.toDouble / total

  def available = total - used
}

case class ResourceApprovalSuccess(minerals: Int, gas: Int, supply: Int) extends ResourceApproval {
  def success = true
  override def ifSuccess[T](then: (ResourceApprovalSuccess) => T): T = then(this)
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
  override def ifSuccess[T](then: (ResourceApprovalSuccess) => T): T = {
    // sorry
    null.asInstanceOf[T]
  }
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
  def forUpgrade(upgrader: Upgrader, price: UpgradePrice, priority: Priority = Priority.Upgrades) = {
    val upgrade = price.forUpgrade
    val mins = price.nextMineralPrice
    val gas = price.nextGasPrice
    ResourceRequests(Seq(MineralsRequest(mins), GasRequest(gas)), priority, upgrader.getClass)

  }
  def forUnit[T <: WrapsUnit](race: SCRace, unspecificType: Class[_ <: T], priority: Priority = Priority.Default) = {
    val unitType = race.specialize(unspecificType)
    val mins = unitType.toUnitType.mineralPrice()
    val gas = unitType.toUnitType.gasPrice()
    val supply = unitType.toUnitType.supplyRequired()

    ResourceRequests(Seq(MineralsRequest(mins), GasRequest(gas), SupplyRequest(supply)), priority, unitType)
  }
}

case class LockedResources[T <: WrapsUnit](reqs: ResourceRequests, employer: Employer[T]) {
  def whatFor = reqs.whatFor
}

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


