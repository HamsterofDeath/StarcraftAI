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
  def forceLock_![T <: WrapsUnit](req: ResourceRequests, employer: Employer[T]) = {
    if (!isAlreadyForceLocked(req, employer)) {
      forceLockInternal_!(req, employer)
    }
  }

  def forceLockInternal_![T <: WrapsUnit](req: ResourceRequests, employer: Employer[T]) = {
    if (!isAlreadyForceLocked(req, employer)) {
      lockedWithoutFunds += LockedResources(req, employer)
    }
  }

  def isAlreadyForceLocked[T <: WrapsUnit](req: ResourceRequests, employer: Employer[T]) = {
    val compareTo = LockedResources(req, employer)
    lockedWithoutFunds.contains(compareTo)
  }

  private val resourceHistory         = ArrayBuffer.empty[MinsGas]
  private val empty                   = Resources(0, 0, Supplies(0, 0))
  private val locked                  = ArrayBuffer.empty[LockedResources[_]]
  private val lockedWithoutFunds      = ArrayBuffer.empty[LockedResources[_]]
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
  def unlock_!(proofForFunding: ResourceApprovalSuccess): Unit = {
    // forced locks first
    val isForced = lockedWithoutFunds.exists(_.reqs.sum.equalValue(proofForFunding))
    if (isForced) {
      lockedWithoutFunds.removeFirstMatch(_.reqs.sum.equalValue(proofForFunding))
    } else {
      locked.removeFirstMatch(_.reqs.sum.equalValue(proofForFunding))
    }
    trace(s"Unlocked $proofForFunding")
    lockedSums.invalidate()
  }

  def forceUnlock_![T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T]) = {
    forceUnlockInternal_!(requests, employer)
  }

  private def forceUnlockInternal_![T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T]) = {
    val lock = LockedResources(requests, employer)
    assert(isAlreadyForceLocked(requests, employer))
    lockedWithoutFunds -= LockedResources(requests, employer)
  }

  def request[T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T], lock: Boolean = true) = {
    val forcedLocked = isAlreadyForceLocked(requests, employer)
    if (forcedLocked) {
      forceUnlockInternal_!(requests, employer)
    }
    val result = {
      trace(s"Incoming resource request: $requests")
      // first check if we have enough resources
      val hasEnoughDespiteLocking = myUnlockedResources.asSum.canCoverCost(requests.sum)
      def approvalFail = {
        failedToProvideThisTick += requests
        ResourceApprovalFail
      }
      if (hasEnoughDespiteLocking) {
        if (lock) lock_!(requests, employer)
        ResourceApprovalSuccess(requests.sum)
      } else {
        val mightGatherEnough = myResources.asSum canCoverCost requests.sum
        if (mightGatherEnough) {
          // check if enough resources could be unlocked
          val unlockOrder = detailedLocks
                            .filter(requests.priority > _.priority)
                            .sortBy { e =>
                              (e.reqs.priority, e.reqs.sum.mineralGasSum)
                            }
          var freed = ResourceRequestSums(0, 0, 0)
          val available = myUnlockedResources.asSum
          def needsMore = !((freed + available) canCoverCost requests.sum)
          val requiredToFree = unlockOrder.takeWhile { resourcesToFree =>
            val stillNeedsMore = needsMore
            if (stillNeedsMore) {
              freed += resourcesToFree
            }
            stillNeedsMore
          }
          if (needsMore) {
            approvalFail
          } else {
            //now ask each locking job if they really allow the unlocking
            val pool = collection.mutable.HashSet.empty ++ requiredToFree
            val notGreedy = unitManager.allFundedJobs.filter {_.canReleaseResources}
            val matched = notGreedy.map { j =>
              val foundMatch = pool.find(_ == j.proofForFunding)
              foundMatch.foreach { e =>
                pool -= e
              }
              j -> foundMatch
            }

            val ok = matched.nonEmpty && matched.forall(_._2.isDefined)
            if (ok) {
              trace(s"Unlocking a bunch of resources ${matched.mkString(", ")} to satisfy ${requests} of $employer")
              matched.foreach { case (j, _) =>
                j.unlockManually_!()
                val targetUnit = j.unit
                unitManager.nobody.assignJob_!(new BusyDoingNothing(targetUnit, unitManager.nobody))
              }
              if (lock) lock_!(requests, employer)
              ResourceApprovalSuccess(requests.sum)

            } else {
              approvalFail
            }
          }
        } else {
          approvalFail
        }
      }
    }
    if (forcedLocked && result.failed) {
      forceLockInternal_!(requests, employer)
    }
    result
  }
  def detailedLocks = locked.toSeq
  private def lock_![T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T]): Unit = {
    val newLock = LockedResources(requests, employer)
    trace(s"Locked $newLock")
    locked += newLock
    lockedSums.invalidate()
  }
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
  private def myUnlockedResources = myResources - lockedResources
  def lockedResources = lockedSums.get
  def plannedSuppliesToAdd = {
    unitManager
    .selectJobs((e: ConstructBuilding[WorkerUnit, Building]) => e.typeOfBuilding == unitManager.race.supplyClass)
  }
  private def calcLockedSums = {
    val normallyLocked = locked.foldLeft(ResourceRequestSums.empty)(_ + _)
    val highPrioLocks = lockedWithoutFunds.foldLeft(ResourceRequestSums.empty)(_ + _)
    normallyLocked + highPrioLocks
  }
}

trait ResourceApproval {
  def minerals: Int
  def gas: Int
  def supply: Int
  def success: Boolean
  def failed = !success
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
  def equalTo(req: ResourceRequests, employer: Employer[T]) = req == reqs && this.employer == employer

  def priority = reqs.priority

  def whatFor = reqs.whatFor
}

case class ResourceRequestSums(minerals: Int, gas: Int, supply: Int) {
  def canCoverCost(other: ResourceRequestSums) = (minerals >= other.minerals || other.minerals == 0) &&
                                                 (gas >= other.gas || other.gas == 0) &&
                                                 (supply >= other.supply || other.supply == 0)

  def +(other: ResourceRequestSums) = ResourceRequestSums(minerals + other.minerals, gas + other.gas,
    supply + other.supply)

  def mineralGasSum: Int = minerals + gas

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
  implicit val ord = Ordering.by[ResourceRequestSums, Int](_.mineralGasSum)
}


