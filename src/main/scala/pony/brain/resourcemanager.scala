package pony
package brain

import pony.brain.modules.UpgradePrice

import scala.collection.mutable
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
  private val resourceAssignmentInfos = mutable.HashMap.empty[ResourceApproval, HasFunding]
  private val resourceHistory         = ArrayBuffer.empty[MinsGas]
  private val empty                       = Resources(0, 0, Supplies(0, 0))
  private val locked                      = ArrayBuffer.empty[LockedResources[_ <: WrapsUnit]]
  private val lockedWithoutFunds          = ArrayBuffer.empty[LockedResources[_ <: WrapsUnit]]
  private val lockedSums                  = LazyVal.from(calcLockedSums)
  private val failedToProvideThisTick     = ArrayBuffer.empty[ResourceRequests]
  private var myResources                 = empty
  private var failedToProvideLastTick     = Vector.empty[ResourceRequests]
  private val lastResortGarbageCollection = mutable.HashSet.empty[(JobHasFunding[_], Int)]

  override def onTick_!() = {
    super.onTick_!()

    lastResortGarbageCollection.filter(_._2 + 24 < currentTick)
    .filter(_._1.stillLocksResources)
    .foreach { unlock =>
      unlock._1.unlockManually_!()
      warn(s"Had to force unlock resources of garbage job ${unlock._1}")
    }

    val obsolete = resourceAssignmentInfos.values
                   .collect { case j: JobHasFunding[_] if j.failedOrObsolete => j }

    val newObsolete = obsolete.filterNot(e => lastResortGarbageCollection.exists(_._1 == e))
    lastResortGarbageCollection ++= newObsolete.map(e => e -> currentTick)
  }

  def informUsage[T <: WrapsUnit](proofForFunding: ResourceApproval, funded: HasFunding) = {
    assert(hasStillLocked(proofForFunding), s"$funded expected to have access to $proofForFunding")
    if (resourceAssignmentInfos.contains(proofForFunding)) {
      trace(s"Holder of $proofForFunding changes from ${resourceAssignmentInfos(proofForFunding)}",
        marker = "MONEY")
    }
    resourceAssignmentInfos.put(proofForFunding, funded)
    trace(s"Holder of $proofForFunding changed to $funded", marker = "MONEY")
  }

  def hasStillLocked(funding: ResourceApproval) = detailedLocks.exists(_.proof.contains(funding))

  def detailedLocks = locked.immutableView

  def couldAffordNow(e: SCUnitType): Boolean = {
    unlockedResources >
    (e.toUnitType.mineralPrice(), e.toUnitType.gasPrice(), e.toUnitType.supplyRequired())
  }

  def forceLock_![T <: WrapsUnit](req: ResourceRequests, employer: Employer[T]) = {
    if (!isAlreadyForceLocked(req, employer)) {
      forceLockInternal_!(req, employer)
    }
  }

  def forceLockInternal_![T <: WrapsUnit](req: ResourceRequests, employer: Employer[T]) = {
    if (!isAlreadyForceLocked(req, employer)) {
      lockedWithoutFunds += LockedResources(req, None, employer)
    }
    lockedSums.invalidate()
  }

  def isAlreadyForceLocked[T <: WrapsUnit](req: ResourceRequests, employer: Employer[T]) = {
    val compareTo = LockedResources(req, None, employer)
    lockedWithoutFunds.contains(compareTo)
  }

  def forceLocks = lockedWithoutFunds.toList

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
    trace(s"Unlocking $proofForFunding")
    val isForced = lockedWithoutFunds.exists(_.reqs.sum.equalValue(proofForFunding))
    if (isForced) {
      lockedWithoutFunds.removeFirstMatch(_.reqs.sum.equalValue(proofForFunding))
      //assert(!locked.exists(_.proof.contains(proofForFunding)), s"Duplicate lock:
      // $proofForFunding")
    } else {
      locked.removeFirstMatch(_.proof.contains(proofForFunding))
      assert(!locked.exists(_.proof.contains(proofForFunding)), s"Duplicate lock: $proofForFunding")
      resourceAssignmentInfos.remove(proofForFunding)
    }
    lockedSums.invalidate()
    trace(s"Unlocked $proofForFunding")

  }

  def forceUnlock_![T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T]) = {
    forceUnlockInternal_!(requests, employer)
  }

  private def forceUnlockInternal_![T <: WrapsUnit](requests: ResourceRequests,
                                                    employer: Employer[T]) = {
    val lock = LockedResources(requests, None, employer)
    assert(isAlreadyForceLocked(requests, employer))
    lockedWithoutFunds -= LockedResources(requests, None, employer)
    lockedSums.invalidate()
  }

  def request[T <: WrapsUnit](requests: ResourceRequests, employer: Employer[T],
                              lock: Boolean = true) = {
    val forcedLocked = isAlreadyForceLocked(requests, employer)
    if (forcedLocked) {
      forceUnlockInternal_!(requests, employer)
    }
    val result = {
      trace(s"Incoming resource request: $requests")
      // first check if we have enough resources
      val hasEnoughDespiteLocking = unlockedResources.asSum.canCoverCost(requests.sum)
      def approvalFail = {
        failedToProvideThisTick += requests
        ResourceApprovalFail
      }
      if (hasEnoughDespiteLocking) {
        val ret = ResourceApprovalSuccess(requests.sum)
        if (lock) lock_!(requests, Some(ret), employer)
        ret
      } else {
        val mightGatherEnough = myResources.asSum canCoverCost requests.sum
        if (mightGatherEnough) {
          val freeableResources = {
            // check if enough resources could be unlocked
            val unlockOrder = detailedLocks
                              .filter(requests.priority > _.priority)
                              .sortBy { e =>
                                (e.reqs.priority, -e.reqs.sum.mineralGasSum)
                              }
            val abortableJobs = unitManager.allJobsWithReleaseableResources
            val used = mutable.HashSet.empty[JobHasFunding[_]]
            unlockOrder.flatMap { locked =>
              val ret = abortableJobs.iterator
                        .filter(!used(_))
                        .find(e => locked.proof.contains(e.proofForFunding))
              used ++= ret
              ret.map(e => e -> locked)
            }
          }

          var freed = ResourceRequestSum(0, 0, 0)
          val available = unlockedResources.asSum
          def needsMore = !((freed + available) canCoverCost requests.sum)
          val requiredToFree = freeableResources.takeWhile { case (job, singleLocked) =>
            val stillNeedsMore = needsMore
            if (stillNeedsMore) {
              freed += singleLocked.reqs.sum
            }
            stillNeedsMore
          }
          if (needsMore) {
            approvalFail
          } else {
            val ok = requiredToFree.nonEmpty
            if (ok) {
              trace(s"Unlocking a bunch of resources ${
                requiredToFree.map(_._2).mkString(", ")
              } to satisfy $requests of $employer")
              requiredToFree.foreach { case (j, unlockable) =>
                val before = detailedLocks.count(_ == unlockable)
                j.notifyResourcesDisapproved_!()
                val after = detailedLocks.count(_ == unlockable)
                assert(before - 1 == after, s"Failed unlocking of $unlockable of $j")
              }
              val ret = ResourceApprovalSuccess(requests.sum)
              if (lock) lock_!(requests, Some(ret), employer)
              ret

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
    trace(s"Result: $result")
    result
  }

  private def lock_![T <: WrapsUnit](requests: ResourceRequests,
                                     proof: Option[ResourceApprovalSuccess],
                                     employer: Employer[T]): Unit = {
    val newLock = LockedResources(requests, proof, employer)
    assert(!isAlreadyForceLocked(requests, employer), s"Lock aready force locked: $requests")
    trace(s"Locked $newLock")
    locked += newLock
    lockedSums.invalidate()
  }

  def unlockedResources = myResources - lockedResources

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

    locked.foreach { lockedResource =>
      val ok = resourceAssignmentInfos.exists { case (proof, job) =>
        lockedResource.proof.contains(proof)
      }
      assert(ok, s"Resources $lockedResource are locked, but no matching job/request is known!")
    }
  }

  def gatheredMinerals = nativeGame.self().gatheredMinerals()

  def gatheredGas = nativeGame.self().gatheredGas()

  def currentResources = myResources

  def supplies = unlockedResources.supply

  def plannedSuppliesToAdd = {
    unitManager
    .selectJobs((e: ConstructBuilding[WorkerUnit, Building]) => e.typeOfBuilding ==
                                                                unitManager.race.supplyClass)
  }

  private def calcLockedSums = {
    val normallyLocked = locked.foldLeft(ResourceRequestSum.empty)(_ + _)
    val highPrioLocks = lockedWithoutFunds.foldLeft(ResourceRequestSum.empty)(_ + _)
    normallyLocked + highPrioLocks
  }
}

trait ResourceApproval {
  lazy val sum = ResourceRequestSum(minerals, gas, supply)
  def minerals: Int
  def gas: Int
  def supply: Int
  def success: Boolean
  def failed = !success
  def isSuccess = success
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

case class ResourceApprovalSuccess(minerals: Int, gas: Int, supply: Int,
                                   uniqueId: ResourceApprovalId)
  extends ResourceApproval {
  def success = true

  override def ifSuccess[T](thenDo: (ResourceApprovalSuccess) => T): T = thenDo(this)
}

case class ResourceApprovalId(i: Int)

object ResourceApprovalSuccess {
  private var counter = 0

  def apply(sums: ResourceRequestSum): ResourceApprovalSuccess = {
    counter += 1
    ResourceApprovalSuccess(sums.minerals, sums.gas,
      sums.supply, ResourceApprovalId(counter))
  }
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

  override def toString = "Not enough resources"
}

trait ResourceRequest {
  def amount: Int
}

case class MineralsRequest(amount: Int) extends ResourceRequest

case class GasRequest(amount: Int) extends ResourceRequest

case class SupplyRequest(amount: Int) extends ResourceRequest

case class ResourceRequests(requests: Seq[ResourceRequest], priority: Priority, whatFor: Class[_]) {
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

  def forUpgrade(upgrader: Upgrader, price: UpgradePrice,
                 priority: Priority = Priority.Upgrades) = {
    val upgrade = price.forUpgrade
    val mins = price.nextMineralPrice
    val gas = price.nextGasPrice
    ResourceRequests(Seq(MineralsRequest(mins), GasRequest(gas)), priority, upgrader.getClass)

  }

  def forUnit[T <: WrapsUnit](race: SCRace, unspecificType: Class[_ <: T],
                              priority: Priority = Priority.Default) = {
    val unitType = race.specialize(unspecificType)
    val mins = unitType.toUnitType.mineralPrice()
    val gas = unitType.toUnitType.gasPrice()
    val supply = unitType.toUnitType.supplyRequired()

    val requestDetails = Seq(MineralsRequest(mins), GasRequest(gas), SupplyRequest(supply))
    ResourceRequests(requestDetails, priority, unitType)
  }
}

case class LockedResources[T <: WrapsUnit](reqs: ResourceRequests,
                                           proof: Option[ResourceApprovalSuccess],
                                           employer: Employer[T]) {
  def equalTo(req: ResourceRequests, employer: Employer[T]) = req == reqs &&
                                                              this.employer == employer

  def priority = reqs.priority

  def whatFor = reqs.whatFor
}

case class ResourceRequestSum(minerals: Int, gas: Int, supply: Int) {
  def canCoverCost(other: ResourceRequestSum) =
    (minerals >= other.minerals || other.minerals == 0) &&
    (gas >= other.gas || other.gas == 0) &&
    (supply >= other.supply || other.supply == 0)

  def +(other: ResourceRequestSum) = ResourceRequestSum(minerals + other.minerals, gas + other.gas,
    supply + other.supply)

  def mineralGasSum: Int = minerals + gas

  def equalValue(proof: ResourceApprovalSuccess) = minerals == proof.minerals && gas == proof.gas &&
                                                   supply == proof.supply

  def +(e: LockedResources[_]): ResourceRequestSum = {
    val sum = e.reqs.sum
    copy(minerals = minerals + sum.minerals, gas = gas + sum.gas, supply = supply + sum.supply)
  }

  def +(e: ResourceRequest): ResourceRequestSum = {
    e match {
      case m: MineralsRequest => copy(minerals = minerals + m.amount)
      case g: GasRequest => copy(gas = gas + g.amount)
      case s: SupplyRequest => copy(supply = supply + s.amount)
    }
  }
}

case class MinsGas(mins: Int, gas: Int)

object ResourceRequestSum {
  val empty = ResourceRequestSum(0, 0, 0)
  implicit val ord = Ordering.by[ResourceRequestSum, Int](_.mineralGasSum)
}


