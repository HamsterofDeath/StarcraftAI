package pony
package brain

import bwapi.Order
import pony.Orders.Stop
import pony.brain.modules.AlternativeBuildingSpot

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class UnitManager(override val universe: Universe) extends HasUniverse {
  def employerOf(unit: WrapsUnit) = {
    assignments.get(unit).flatMap { job =>
      byEmployer.find { e =>
        val jobsOfEmployer = e._2
        jobsOfEmployer.contains(job)
      }
    }.map(_._1)
  }

  def hasJob(support: OrderHistorySupport) = assignments.contains(support)

  private val reorganizeJobQueue          = ListBuffer.empty[CanAcceptUnitSwitch[_ <: WrapsUnit]]
  private val unfulfilledRequestsThisTick = ArrayBuffer.empty[UnitJobRequests[_ <: WrapsUnit]]
  private val assignments                 = mutable.HashMap.empty[WrapsUnit, UnitWithJob[_ <: WrapsUnit]]
  private val byEmployer                  = multiMap[Employer[_ <: WrapsUnit], UnitWithJob[_ <: WrapsUnit]]
  private var unfulfilledRequestsLastTick = unfulfilledRequestsThisTick.toVector
  def allIdleMobiles = allJobsByType[BusyDoingNothing[Mobile]].filter(_.unit.isInstanceOf[Mobile])
  def allJobsByType[T <: UnitWithJob[_] : Manifest] = {
    val wanted = manifest[T].runtimeClass
    assignments.valuesIterator.filter { job =>
      wanted.isAssignableFrom(job.getClass)
    }.map {_.asInstanceOf[T]}.toVector
  }
  def allFundedJobs = assignments.values.collect { case f: JobHasFunding[_] => f }
  def existsOrPlanned(c: Class[_ <: WrapsUnit]) = {
    ownUnits.ownsByType(c) ||
    plannedToBuild.exists(e => c.isAssignableFrom(e.typeOfRequestedUnit)) ||
    plannedToTrain.exists(e => c.isAssignableFrom(e.typeOfRequestedUnit))
  }
  def plannedToTrain = allUnfulfilled.flatMap(_.requests)
                       .collect { case t: BuildUnitRequest[_] if t.isMobile => t }
  def existsAndDone(c: Class[_ <: WrapsUnit]) = {
    ownUnits.existsComplete(c)
  }
  def nextJobReorganisationRequest = {
    val ret = reorganizeJobQueue.lastOption.filterNot(_.hasFailed)
    if (reorganizeJobQueue.nonEmpty) {
      reorganizeJobQueue.remove(0)
    }
    trace(s"${reorganizeJobQueue.size} jobs left to optimize")
    ret
  }
  def tryFindBetterEmployeeFor[T <: WrapsUnit](anyJob: CanAcceptUnitSwitch[T]): Unit = {
    reorganizeJobQueue += anyJob
  }
  def plannedToBuild(c: Class[_ <: Building]): Boolean = plannedToBuild
                                                         .exists(e => c.isAssignableFrom(e.typeOfRequestedUnit))
  def plannedToBuild = allUnfulfilled.flatMap(_.requests)
                       .collect { case b: BuildUnitRequest[_] if b.isBuilding => b }
  private def allUnfulfilled = unfulfilledRequestsLastTick.toSet ++ unfulfilledRequestsThisTick.toSet
  def plannedToBuildByType[T <: Building : Manifest]: Int = {
    val typeOfFactory = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]
    unfulfilledByTargetType(typeOfFactory).size
  }
  def plannedToBuildByClass(typeOfFactory: Class[_ <: Building]) = {
    unfulfilledByTargetType(typeOfFactory)
  }
  def requestedConstructions[T <: Building : Manifest] = {
    val typeOfFactory = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]
    unfulfilledByTargetType(typeOfFactory)
  }
  private def unfulfilledByTargetType[T <: WrapsUnit](targetType: Class[_ <: T]) = {
    allUnfulfilled.flatMap(_.requests).iterator.collect {
      case b: BuildUnitRequest[_] if b.typeOfRequestedUnit == targetType => b
    }
  }
  def constructionsInProgress[T <: Building : Manifest]: Seq[ConstructBuilding[WorkerUnit, T]] = {
    constructionsInProgress(manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]])
  }
  def constructionsInProgress[T <: Building](typeOfBuilding: Class[_ <: T]): Seq[ConstructBuilding[WorkerUnit, T]] = {
    val byJob = allJobsByType[ConstructBuilding[WorkerUnit, Building]].collect {
      case cr: ConstructBuilding[WorkerUnit, Building] if typeOfBuilding.isAssignableFrom(cr.typeOfBuilding) =>
        cr.asInstanceOf[ConstructBuilding[WorkerUnit, T]]
    }
    byJob
  }
  def unitsByType[T <: WrapsUnit : Manifest]: collection.Set[T] = {
    val runtimeClass = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]
    unitsByType(runtimeClass)
  }
  def unitsByType[T <: WrapsUnit](typeOfFactory: Class[_ <: T]): collection.Set[T] = {
    assignments.keySet.collect {
      case elem: WrapsUnit if typeOfFactory.isAssignableFrom(elem.getClass) => elem.asInstanceOf[T]
    }
  }
  def jobsByType = assignments.values.toSeq.groupBy(_.getClass)
  def jobsOf[T <: WrapsUnit](emp: Employer[T]) = byEmployer.getOrElse(emp, Set.empty)
                                                 .asInstanceOf[collection.Set[UnitWithJob[T]]]
  def jobByUnitIdString(str: String) = assignments.find(_._1.unitIdText == str).map(_._2)
  def employers = byEmployer.keySet
  def plannedSupplyAdditions = {
    val byJob = allJobsByType[ConstructBuilding[WorkerUnit, Building]].collect {
      case cr: ConstructBuilding[WorkerUnit, Building] => cr.typeOfBuilding.toUnitType.supplyProvided()
    }.sum
    val byUnfulfilledRequest = allUnfulfilled.flatMap(_.requests).collect {
      case b: BuildUnitRequest[_] => b.typeOfRequestedUnit.toUnitType.supplyProvided()
    }.sum
    byJob + byUnfulfilledRequest
  }
  def allJobsByUnitType[T <: WrapsUnit : Manifest] = selectJobs[T, UnitWithJob[T]](_ => true)
  def selectJobs[U <: WrapsUnit : Manifest, T <: UnitWithJob[U] : Manifest](f: T => Boolean) = {
    val wanted = manifest[U].runtimeClass
    assignments.valuesIterator.filter { job =>
      wanted.isAssignableFrom(job.unit.getClass) && f(job.asInstanceOf[T])
    }.map {_.asInstanceOf[T]}.toVector
  }
  def failedToProvideByType[T <: WrapsUnit : Manifest] = {
    val c = manifest[T].runtimeClass
    failedToProvideFlat.collect {
      case req: UnitRequest[_] if c.isAssignableFrom(req.typeOfRequestedUnit) => req.asInstanceOf[UnitRequest[T]]
    }
  }
  def failedToProvideFlat = failedToProvide.flatMap(_.requests)
  def failedToProvide = unfulfilledRequestsLastTick.toSeq
  def jobOf[T <: WrapsUnit](unit: T) = assignments(unit).asInstanceOf[UnitWithJob[T]]
  def tick(): Unit = {
    // do not pile these up, clear per tick - this is why unitmanagers tick must come last.
    val (clearable, keep) = unfulfilledRequestsLastTick.partition(_.clearable)
    clearable.foreach(_.onClear())
    unfulfilledRequestsLastTick = unfulfilledRequestsThisTick.toVector ++ keep
    unfulfilledRequestsThisTick.clear()

    //clean/update
    ownUnits.all.foreach(_.onTick())
    assignments.valuesIterator.foreach(_.onTick())
    enemies.all.foreach(_.onTick())
    val removeUs = {
      val done = assignments.filter { case (_, job) => job.isFinished }.values
      val failed = assignments.filter { case (_, job) => job.hasFailed }.values

      trace(s"${failed.size}/${done.size} jobs failed/finished, putting units on the market again",
        failed.nonEmpty || done.nonEmpty)

      failed.foreach { failure =>
        warn(s"FAIL! $failure")
      }
      trace(s"Failed: ${failed.mkString("\n")}")
      trace(s"Finished: ${done.mkString(", ")}")
      val ret = (done ++ failed).toVector
      assert(ret.size == ret.distinct.size, s"A job is failed and finished at the same time")
      ret
    }
    trace(s"Cleaning up ${removeUs.size} finished/failed jobs", removeUs.nonEmpty)

    removeUs.foreach { job =>
      job.unit match {
        case m: Mobile if !m.isDead =>
          // stop whatever you were doing so the next employer doesn't hire a rebel
          world.orderQueue.queue_!(new Stop(m))
        case _ =>
      }

      job.unit match {
        case cd: CanDie if !cd.isDead =>
          val newJob = new BusyDoingNothing(cd, Nobody)
          assignJob_!(newJob)
        case cd: CanDie if cd.isDead =>
          assignments.remove(cd)
          byEmployer.removeBinding(job.employer, job)
        case res: MineralPatch =>
          assignments.remove(res)
          byEmployer.removeBinding(job.employer, job)
        case _ =>
      }
      job.onFinishOrFail()
    }

    def initialJobOf[T <: WrapsUnit](unit: T) = {
      if (unit.isBeingCreated) {
        unit match {
          case b: Building =>
            new BusyBeingContructed(unit, Constructor)
          case m: Mobile =>
            new BusyBeingTrained(unit, Trainer)
          case _ => throw new UnsupportedOperationException(s"Check this: $unit")
        }
      } else {
        new BusyDoingNothing(unit, Nobody)
      }
    }

    val myOwn = universe.world.myUnits.mine.filterNot(assignments.contains).map(initialJobOf).toSeq
    info(s"Found ${myOwn.size} new units of player", myOwn.nonEmpty)

    myOwn.foreach(assignJob_!)
    assignments ++= myOwn.map(e => e.unit -> e)

    val registerUs = universe.world.myUnits.all.filterNot(assignments.contains).map(initialJobOf).toSeq
    info(s"Found ${registerUs.size} new units (not of player)", registerUs.nonEmpty)
    assignments ++= registerUs.map(e => e.unit -> e)

  }
  def assignJob_![T <: WrapsUnit](newJob: UnitWithJob[T]): Unit = {
    val employer = newJob.employer
    trace(s"New job assignment: $newJob, employed by $employer")
    val newUnit = newJob.unit
    employer.hire_!(newUnit)
    assignments.get(newUnit).foreach { oldJob =>
      assert(oldJob.unit eq newUnit, s"${oldJob.unit} is not $newUnit")
      oldJob.onStealUnit()
      val oldAssignment = byEmployer.find(_._2(oldJob))
      assert(oldAssignment.isDefined)
      oldAssignment.foreach { case (oldEmployer, _) =>
        trace(s"Old job assignment was: $oldJob, employed by $oldEmployer")
        byEmployer.removeBinding(oldEmployer, oldJob)
        val oldEmployerTyped = oldEmployer.asInstanceOf[Employer[T]]
        oldEmployerTyped.hiredBySomeoneMoreImportant_!(newUnit)
      }
    }

    assignments.put(newUnit, newJob)
    byEmployer.addBinding(employer, newJob)
  }

  def allRequirementsFulfilled[T <: WrapsUnit](c: Class[_ <: T]) = {
    val (m, i) = findMissingRequirements[T](collection.immutable.Set(c))
    m.isEmpty && i.isEmpty
  }

  def requirementsQueuedToBuild[T <: WrapsUnit](c: Class[_ <: T]) = {
    val (_, i) = findMissingRequirements[T](collection.immutable.Set(c))
    i.nonEmpty
  }

  private def findMissingRequirements[T <: WrapsUnit](c: Set[Class[_ <: T]]) = {
    val mustHave = c.flatMap(race.techTree.requiredFor)
    val i = mutable.Set.empty[Class[_ <: Building]]
    val m = mutable.Set.empty[Class[_ <: Building]]
    val missing = {
      mustHave.foreach { dependency =>
        val complete = ownUnits.existsComplete(dependency)
        if (!complete) {
          val incomplete = ownUnits.existsIncomplete(dependency)
          if (!incomplete) {
            m += dependency
          } else {
            i += dependency
          }
        }
      }
    }
    (m.toSet, i.toSet)
  }

  def request[T <: WrapsUnit : Manifest](req: UnitJobRequests[T], buildIfNoneAvailable: Boolean = true) = {
    trace(s"${req.employer} requested ${req.requests.mkString(" and ")}")
    def hireResult: Option[UnitCollector[T]] = {
      new UnitCollector(req, universe).collect_!()
    }

    val (missing, incomplete) = findMissingRequirements(req.allRequiredTypes)
    val result = if (missing.isEmpty && incomplete.isEmpty) {
      val hr = hireResult
      hr match {
        case None =>
          if (buildIfNoneAvailable) unfulfilledRequestsThisTick += req
          new FailedPreHiringResult[T]
        case Some(team) if !team.complete =>
          trace(s"Partially successful hiring request: $team")
          if (buildIfNoneAvailable) unfulfilledRequestsThisTick += team.missingAsRequest
          if (team.hasOneMember)
            new PartialPreHiringResult(team.teamAsMap) with ExactlyOneSuccess[T]
          else
            new PartialPreHiringResult(team.teamAsMap)
        case Some(team) =>
          trace(s"Successful hiring request: $team")
          if (team.hasOneMember) {
            new SuccessfulPreHiringResult(team.teamAsMap) with ExactlyOneSuccess[T]
          } else {
            new SuccessfulPreHiringResult(team.teamAsMap)
          }
      }
    } else {
      new MissingRequirementResult[T](missing, incomplete)
    }

    trace(s"Result of request: $result")
    result
  }
  def allOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.getOrElse(employer, Set.empty)
  def allNotOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.filter(_._1 != employer)
                                                                .flatMap(_._2)
  def nobody = Nobody

  case object Nobody extends Employer[WrapsUnit](universe)
  case object Trainer extends Employer[WrapsUnit](universe)
  case object Constructor extends Employer[WrapsUnit](universe)
}

trait PreHiringResult[T <: WrapsUnit] {
  def hasAnyMissingRequirements = notExistingMissingRequiments.nonEmpty || inProgressMissingRequirements.nonEmpty

  def notExistingMissingRequiments: Set[Class[_ <: Building]] = Set.empty
  def inProgressMissingRequirements: Set[Class[_ <: Building]] = Set.empty
  def success: Boolean
  def canHire: Map[UnitRequest[T], Set[T]]
  def units = canHire.flatMap(_._2)

  override def toString = s"HiringResult($success, $canHire)"

  def ifOne[X](todo: T => X) = this match {
    case one: ExactlyOneSuccess[T] => todo(one.onlyOne)
    case _ =>
  }

  def ifNotZero[X](todo: Seq[T] => X) = this match {
    case many: AtLeastOneSuccess[T] =>
      val canHireThese = many.canHire.valuesIterator.flatten.toVector
      todo(canHireThese)
    case _ =>
  }
}

class FailedPreHiringResult[T <: WrapsUnit] extends PreHiringResult[T] {
  def success = false
  def canHire = Map.empty
}

class MissingRequirementResult[T <: WrapsUnit](needs: Set[Class[_ <: Building]], incomplete: Set[Class[_ <: Building]])
  extends PreHiringResult[T] {
  def success = false
  def canHire = Map.empty
  override def notExistingMissingRequiments = needs
  override def inProgressMissingRequirements = incomplete
}

trait AtLeastOneSuccess[T <: WrapsUnit] extends PreHiringResult[T] {
  def one = canHire.valuesIterator.next().head
}

trait ExactlyOneSuccess[T <: WrapsUnit] extends AtLeastOneSuccess[T] {
  def onlyOne = {
    assert(canHire.size == 1)
    val set = canHire.valuesIterator.next()
    assert(set.size == 1)
    set.head
  }
}

class SuccessfulPreHiringResult[T <: WrapsUnit](override val canHire: Map[UnitRequest[T], Set[T]])
  extends PreHiringResult[T] with AtLeastOneSuccess[T] {
  def success = true
}

class PartialPreHiringResult[T <: WrapsUnit](override val canHire: Map[UnitRequest[T], Set[T]])
  extends PreHiringResult[T] with AtLeastOneSuccess[T] {
  def success = false
}

class UnitCollector[T <: WrapsUnit : Manifest](req: UnitJobRequests[T], override val universe: Universe)
  extends HasUniverse {

  private val hired                      = multiMap[UnitRequest[T], T]
  private val remainingAmountsPerRequest = mutable.HashMap.empty ++=
                                           req.requests.map { e => e -> e.amount }.toMap
  def onlyMember = {
    assert(hasOneMember)
    hired.values.head.head
  }
  def hasOneMember = hired.valuesIterator.map(_.size).sum == 1
  def collect_!(includeCandidates: Seq[UnitWithJob[T]] = Nil): Option[UnitCollector[T]] = {
    val um = unitManager
    val available = {
      val potential = {
                        val all = um.allOfEmployer(um.Nobody).iterator ++ um.allNotOfEmployer(um.Nobody).iterator
                        val requested = all.filter(_.unit.isInGame).filter(requests)
        val withoutHigherPrio = requested
                                .filter(_.priority < req.priority)
        val withInterruptable = withoutHigherPrio.filter(interrupts)
        val withExplicitCandidates = withInterruptable ++ includeCandidates
        withExplicitCandidates.map(typed)
                      }.toVector
      priorityRule.fold(potential) { rule =>
        potential.sortBy(rule.giveRating)
      }
    }

    available.foreach { candidate =>
      collect(candidate.unit)

      if (complete) {
        return Some(this)
      }
    }


    if (hasAny)
      Some(this)
    else
      None
  }
  def typed(any: UnitWithJob[_ <: WrapsUnit]) = any.asInstanceOf[UnitWithJob[T]]
  def hasAny = hired.nonEmpty
  def collect(unit: WrapsUnit) = {
    remainingAmountsPerRequest.find {
      case (request, remaining) if remaining > 0 && request.includesByType(unit) => true
      case _ => false
    }.foreach { case (request, missing) =>
      remainingAmountsPerRequest.put(request, missing - 1)
      // we know the type because we asked req before
      hired.addBinding(request, unit.asInstanceOf[T])
    }
  }
  def complete = remainingAmountsPerRequest.valuesIterator.sum == 0
  def requests(unit: UnitWithJob[_ <: WrapsUnit]) = {
    req.wantsUnit(unit.unit)
  }
  def interrupts(unit: UnitWithJob[_ <: WrapsUnit]) = {
    req.canInterrupt(unit)
  }
  def hasPriorityRule = priorityRule.isDefined
  def priorityRule = req.priorityRule
  def missingAsRequest: UnitJobRequests[T] = {
    val typesAndAmounts = remainingAmountsPerRequest.filter(_._2 > 0).map { case (req, amount) =>
      BuildUnitRequest[T](universe, req.typeOfRequestedUnit, amount, ResourceApprovalFail, Priority.Default,
      AlternativeBuildingSpot.useDefault)
    }
    UnitJobRequests[T](typesAndAmounts.toSeq, req.employer, req.priority)
  }
  override def toString: String = s"Collected: $teamAsMap"

  def teamAsMap = hired.toSeq.map { case (k, v) => k -> v.toSet }.toMap
}

class Employer[T <: WrapsUnit : Manifest](override val universe: Universe) extends HasUniverse {
  self =>
  private var employees = ArrayBuffer.empty[T]

  universe.register_!(() => {
    employees.filterNot(_.isInGame).foreach(fire_!)
  })

  def teamSize = employees.size

  def idleHiredUnits = employees.filter(unitManager.jobOf(_).isIdle)

  def hire_!(result: PreHiringResult[T]): Unit = {
    result.units.foreach(hire_!)
  }

  def hire_!(unit: T): Unit = {
    assert(!employees.contains(unit), s"$unit already in $this doing ${unitManager.jobOf(unit)}")
    info(s"$unit got hired by $this")
    employees += unit
  }

  def fire_!(unit: T): Unit = {
    assert(employees.contains(unit), s"$unit not in $this")
    info(s"$unit got fired")
    employees -= unit
  }

  def hiredBySomeoneMoreImportant_!(unit: T): Unit = {
    assert(employees.contains(unit), s"$unit not in $this")
    trace(s"$unit got hired by someone else")
    employees -= unit
  }

  def assignJob_!(job: UnitWithJob[T]): Unit = {
    assert(!employees.contains(job.unit),
      s"Already hired ${job.unit} by $this, cannot give it new job $job because it already has ${
        unitManager.jobOf(job.unit)
      }")
    assert(this == job.employer, s"$this is not ${job.employer}")
    unitManager.assignJob_!(job)
  }

  def current = employees.toSeq
}

trait CanAcceptUnitSwitch[T <: WrapsUnit] extends UnitWithJob[T] {
  def newFor(replacement: T): UnitWithJob[T]

  def asRequest: UnitJobRequests[T]
  def canSwitchNow: Boolean
  def couldSwitchInTheFuture: Boolean
  def hasToSwitchLater = !canSwitchNow && couldSwitchInTheFuture
}

trait Interruptable {
  def interruptableNow = true
}

object JobCounter {
  private var jobs = 0
  def next() = {
    jobs += 1
    jobs
  }
}


abstract class UnitWithJob[T <: WrapsUnit](val employer: Employer[T], val unit: T, val priority: Priority)
  extends HasUniverse {

  private val myJobId = JobCounter.next()

  private var forceFail = false
  protected def fail() = {
    forceFail = true
  }


  def renderDebug(renderer: Renderer): Unit = {}

  protected def omitRepeatedOrders = false

  unit match {
    case cd: CanDie if cd.isDead => fail()
    case _ =>
  }

  override val universe           = employer.universe
  private  val creationTick       = currentTick
  private  val listeners          = ArrayBuffer.empty[JobFinishedListener[T]]
  private  var noCommandsForTicks = 0

  private var dead = false

  ownUnits.registerKill_!(OnKillListener.on(unit, () => {
    trace(s"Unit $unit died, aborting $this")
    dead = true
  }))

  def hasNotYetSpendResources: Boolean = true
  def onTick(): Unit = {

  }
  def onStealUnit() = {}
  def shortDebugString: String
  def age = currentTick - creationTick
  def isIdle: Boolean = false

  private var lastOrder: Seq[UnitOrder] = Nil
  def ordersForThisTick = {
    if (hasFailed) {
      Nil
    } else {
      if (noCommandsForTicks > 0) {
        noCommandsForTicks -= 1
        Nil
      } else {
        noCommandsForTicks = everyNth
        val nextOrder = ordersForTick
        if (!nextOrder.exists(_.forceRepetition) &&
            omitRepeatedOrders &&
            nextOrder == lastOrder) {
          Nil
        } else {
          lastOrder = nextOrder
          nextOrder
        }
      }
    }
  }
  def everyNth = 0
  def noCommandsForTicks_!(n: Int): Unit = {
    noCommandsForTicks = n
  }
  override def toString: String = s"[J#$myJobId] ${getClass.className} of $unit of $employer"
  def isFinished: Boolean
  def onFinishOrFail(): Unit = {
    listeners.foreach(_.onFinishOrFail(hasFailed))
  }
  def listen_!(listener: JobFinishedListener[T]): Unit = listeners += listener
  def hasFailed = dead || forceFail || jobHasFailedWithoutDeath

  def jobHasFailedWithoutDeath: Boolean = false
  protected def ordersForTick: Seq[UnitOrder]
}

trait IssueOrderNTimes[T <: WrapsUnit] extends UnitWithJob[T] {
  private var issued = 0
  def getOrder: Seq[UnitOrder]
  override def ordersForTick: Seq[UnitOrder] = {
    if (issued == times)
      Nil
    else {
      issued += 1
      getOrder
    }
  }
  def times = 1
}

trait HasFunding {
  private var explicitlyUnlocked = false
  private var autoUnlocked       = false
  def proofForFunding: ResourceApproval
  def resources: ResourceManager
  def stillLocksResources = !explicitlyUnlocked && !autoUnlocked
  def notifyResourcesDisapproved_!(): Unit = {
    trace(s"Resources of $this just got disapproved")
    unlockManually_!()
  }
  private var unlockedDebug        : Any = null
  private var unlockedManuallyDebug: Any = null

  def unlockManually_!(): Unit = {
    assert(!explicitlyUnlocked, s"Don't do that twice")
    assert(!autoUnlocked, s"Too slow")
    trace(s"Manually unlocking $proofForFunding of $this")
    unlock_!()
    explicitlyUnlocked = true
    unlockedManuallyDebug = Thread.currentThread().getStackTrace
  }
  def unlock_!(): Unit = {
    if (!explicitlyUnlocked) {
      assert(!autoUnlocked, s"Already unlocked: $this")
      proofForFunding match {
        case suc: ResourceApprovalSuccess =>
          autoUnlocked = true
          resources.unlock_!(suc)
          unlockedDebug = Thread.currentThread().getStackTrace
        case _ =>
      }
    } else {
      trace(s"Not unlocking $proofForFunding of $this automatically, someone already did that")
    }
  }
}

trait JobHasFunding[T <: WrapsUnit] extends UnitWithJob[T] with HasUniverse with HasFunding {
  self =>

  assert(proofForFunding.isFunded, s"Problem, check $this")

  def canReleaseResources = age == 0 && stillLocksResources

  override def notifyResourcesDisapproved_!(): Unit = {
    super.notifyResourcesDisapproved_!()
    fail()
  }

  listen_!(failed => {
    trace(s"Unlock because $self failed")
    unlock_!()
  })
}

trait JobFinishedListener[T <: WrapsUnit] {
  def onFinishOrFail(failed: Boolean): Unit
}

trait CreatesUnit[T <: WrapsUnit] extends UnitWithJob[T]

class TrainUnit[F <: UnitFactory, T <: Mobile](factory: F, trainType: Class[_ <: T], employer: Employer[F],
                                               funding: ResourceApprovalSuccess)
  extends UnitWithJob[F](employer, factory, Priority.Default) with JobHasFunding[F] with IssueOrderNTimes[F] with
          CreatesUnit[F] {

  private var startedToProduce = false
  override def proofForFunding = funding
  override def getOrder: Seq[UnitOrder] = {
    assert(!hasFailed, s"$this has failed, but is still asked for its order")
    info(s"Training $trainType")
    Orders.Train(unit, trainType).toSeq
  }

  override def onTick(): Unit = {
    super.onTick()

    if (!startedToProduce && age > 20 && factory.isProducing) {
      startedToProduce = true
      trace(s"Job $this starte to produce ${trainType.className}")
      unlockManually_!()
    }
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    age > 50 && !startedToProduce
  }

  override def isFinished = {
    val idle = !factory.isProducing
    val isLazy = !factory.nativeUnit.isTraining
    val ret = idle && isLazy && startedToProduce
    ret
  }

  override def shortDebugString: String = s"Train ${trainType.className}"
}

class ConstructAddon[W <: CanBuildAddons, A <: Addon](employer: Employer[W],
                                                      basis: W,
                                                      what: Class[_ <: A],
                                                      funding: ResourceApproval)
  extends UnitWithJob[W](employer, basis, Priority.Addon) with JobHasFunding[W] with CreatesUnit[W] with
          IssueOrderNTimes[W] {
  assert(!basis.isBuildingAddon)
  assert(!basis.hasCompleteAddon)
  assert(!basis.hasAddonAttached)

  private var startedConstruction = false
  private var stoppedConstruction = false

  override def times: Int = 10

  override def shortDebugString: String = s"Construct ${builtWhat.className}"
  private def builtWhat = what
  override def isFinished: Boolean = {
    startedConstruction && stoppedConstruction
  }
  override def onTick(): Unit = {
    super.onTick()
    if (!startedConstruction) {
      startedConstruction = basis.isBuildingAddon
    }
    if (startedConstruction && !stoppedConstruction) {
      val addon = basis.hasCompleteAddon
      stoppedConstruction = addon
    }
  }
  override def onFinishOrFail(): Unit = {
    super.onFinishOrFail()
    universe.myUnits.allAddons.find(e => basis.positionedNextTo(e)).foreach { e =>
      basis.notifyAttach_!(e)
    }
  }
  override def jobHasFailedWithoutDeath: Boolean = {
    def myFail = age > 50 && !startedConstruction
    super.jobHasFailedWithoutDeath || myFail
  }
  override def getOrder = Orders.ConstructAddon(basis, builtWhat).toSeq
  override def proofForFunding = funding
}

class ResearchUpgrade[U <: Upgrader](employer: Employer[U],
                                     basis: U,
                                     what: Upgrade,
                                     funding: ResourceApproval)
  extends UnitWithJob[U](employer, basis, Priority.Upgrades) with JobHasFunding[U] with IssueOrderNTimes[U] {

  private var startedResearch = false
  private var stoppedResearch = false
  override def shortDebugString: String = s"Research $what"
  override def isFinished: Boolean = {
    startedResearch && stoppedResearch
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    super.jobHasFailedWithoutDeath || (age > 50 && !startedResearch)
  }

  override def onTick(): Unit = {
    super.onTick()
    if (!startedResearch && basis.isDoingResearch) {
      startedResearch = true
    }
    if (startedResearch && !basis.isDoingResearch) {
      stoppedResearch = true
    }
  }

  override def getOrder =
    Orders.Research(basis, what).toSeq

  override def proofForFunding = funding

}

class ConstructBuilding[W <: WorkerUnit : Manifest, B <: Building](worker: W, buildingType: Class[_ <: B],
                                                                   employer: Employer[W],
                                                                   val buildWhere: MapTilePosition,
                                                                   funding: ResourceApprovalSuccess,
                                                                   val belongsTo: Option[ResourceArea] = None)
  extends UnitWithJob[W](employer, worker, Priority.ConstructBuilding) with JobHasFunding[W] with CreatesUnit[W] with
          IssueOrderNTimes[W] with CanAcceptUnitSwitch[W] {

  val area = {
    val unitType = buildingType.toUnitType
    val size = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    Area(buildWhere, size)
  }
  assert(resources.detailedLocks.exists(e => e.whatFor == buildingType && e.reqs.sum == funding.sum),
    s"Something is wrong, check $this, it is supposed to have ${
      funding.sum
    } funding, but the resource manager only has\n ${resources.detailedLocks.mkString("\n")}\nlocked")

  private var startedMovingToSite       = false
  private var startedActualConstruction = false
  private var finishedConstruction      = false
  private var resourcesUnlocked         = false
  private var constructs                = Option.empty[Building]

  override def onTick(): Unit = {
    super.onTick()
    if (startedActualConstruction && !resourcesUnlocked && constructs.isDefined) {
      trace(s"Construction job $this no longer needs to lock resources")
      unlockManually_!()
      resourcesUnlocked = true
      mapLayers.unblockBuilding_!(area)
    }
  }

  override def getOrder: Seq[UnitOrder] =
    Orders.ConstructBuilding(worker, buildingType, buildWhere).toSeq
  override def shortDebugString: String = s"Build ${buildingType.className}"
  override def proofForFunding = funding
  override def canSwitchNow = !startedActualConstruction
  override def couldSwitchInTheFuture = !startedActualConstruction
  override def jobHasFailedWithoutDeath: Boolean = {
    val fail = !worker.isInConstructionProcess && age > times + 10 && !isFinished
    warn(s"Construction of ${typeOfBuilding.className} failed, worker $worker didn't manange", fail)
    fail
  }
  def typeOfBuilding = buildingType

  override def isFinished = {
    if (!startedMovingToSite) {
      startedMovingToSite = worker.isInConstructionProcess
      trace(s"Worker $worker started to move to construction site", startedMovingToSite)
    } else if (!startedActualConstruction) {
      startedActualConstruction = worker.isConstructingBuilding
      if (startedActualConstruction) {
        constructs = employer.ownUnits.buildingAt(buildWhere)
        // i saw this going wrong once
        if (constructs.isEmpty) {
          fail()
        }
      }
      trace(s"Worker $worker started to build $buildingType", startedActualConstruction)
    } else if (!finishedConstruction) {
      assert(constructs.isDefined)
      finishedConstruction = !worker.isInConstructionProcess
      trace(s"Worker $worker finished to build $buildingType", finishedConstruction)
    }
    age > times + 10 && startedMovingToSite && finishedConstruction
  }
  override def times = 50
  def building = constructs
  override def asRequest: UnitJobRequests[W] = {
    val request = AnyUnitRequest[W](worker.getClass, 1)
    val anyUnitRequest = request.withCherryPicker_! { hijackFromThis =>
      val altUnit = hijackFromThis.unit
      val sameArea = mapLayers.rawWalkableMap.areaWhichContains(altUnit.currentTile) ==
                     mapLayers.rawWalkableMap.areaWhichContains(buildWhere)

      val canSee = mapLayers.rawWalkableMap.connectedByLine(altUnit.currentTile, buildWhere)

      val distance = area.distanceTo(altUnit.currentTile)
      val busyness = WorkerUnit.currentPriority(hijackFromThis)
      val weightedDistance = distance * busyness.sum

      PriorityChain(sameArea.ifElse(0, 1), canSee.ifElse(0, 1), weightedDistance)
    }
    UnitJobRequests(anyUnitRequest.toSeq, employer, priority)
  }

  override def newFor(replacement: W) = new
      ConstructBuilding(replacement, buildingType, employer, buildWhere, funding, belongsTo)
}

sealed trait Behaviour
case object AggressiveMove extends Behaviour
case object HoldPosition extends Behaviour
case object FallBack extends Behaviour
case object Undefined extends Behaviour

case class TargetPosition(where: MapTilePosition, randomize: Int)
case class Objective(target: Option[TargetPosition], how: Behaviour)

object Objective {
  val initial = Objective(None, Undefined)
}

case class SingleUnitBehaviourMeta(priority: SecondPriority, refuseCommandsForTicks: Int, forceRepeats: Boolean)

abstract class SingleUnitBehaviour[T <: Mobile](val unit: T, meta: SingleUnitBehaviourMeta) {
  def toOrder(what: Objective): Seq[UnitOrder]
  def preconditionOk = true
  def shortName: String
  def priority = meta.priority
  def blocksForTicks = meta.refuseCommandsForTicks
  def forceRepeats = meta.forceRepeats
}

class BusyDoingSomething[T <: Mobile](employer: Employer[T], behaviour: Seq[SingleUnitBehaviour[T]],
                                      private var objective: Objective)
  extends UnitWithJob(employer, behaviour.head.unit, Priority.DefaultBehaviour) with Interruptable {

  assert(behaviour.map(_.unit).distinct.size == 1, s"Wrong grouping: $behaviour")

  def newObjective_!(objective: Objective): Unit = {
    this.objective = objective
  }

  override protected def omitRepeatedOrders = true

  override def shortDebugString = s"(BG) ${active.map(_.shortName).mkString(", ")}"
  private def active = behaviour.filter(_.preconditionOk)
  // never ends
  override def isFinished = false
  override protected def ordersForTick = {
    val options = active.map { rule =>
      rule -> rule.toOrder(objective).map(_.lockingFor_!(rule.blocksForTicks).forceRepeat_!(rule.forceRepeats))
    }.filter(_._2.nonEmpty)
    if (options.isEmpty) {
      Nil
    } else {
      options.maxBy(_._1.priority)._2
    }
  }
}

class BusyDoingNothing[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.None) with IssueOrderNTimes[T] with Interruptable {
  override def isIdle = true

  override def getOrder: Seq[UnitOrder] = {
    unit match {
      case m: Mobile if m.currentOrder != Order.PlayerGuard && m.currentOrder != Order.Stop =>
        Orders.Stop(m).toSeq
      case _ => Nil
    }
  }

  override def times: Int = 5

  override def isFinished = false
  override def shortDebugString: String = "Idle"
}

class BusyBeingTrained[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.Max) {
  override def isIdle = false
  override def ordersForTick: Seq[UnitOrder] = Nil
  override def isFinished = unit.nativeUnit.getRemainingBuildTime == 0
  override def shortDebugString: String = "Train me"
}

class BusyBeingContructed[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.Max) {
  override def isIdle = false
  override def ordersForTick: Seq[UnitOrder] = Nil
  override def isFinished = unit.nativeUnit.getRemainingBuildTime == 0
  override def shortDebugString: String = "Build me"
}

case class PriorityChain(data: Vector[Double]) {
  lazy val sum = data.sum
}
object PriorityChain {

  implicit val ordOnPriorities: Ordering[PriorityChain] = {
    implicit val ordOnVectorWithDoubles: Ordering[Vector[Double]] = Ordering.fromLessThan { (a, b) =>
      assert(a.size == b.size)
      def isLess: Boolean = {
        for (i <- a.indices) {
          val lessThan = a(i) < b(i)
          if (lessThan) return true
        }
        false
      }
      isLess
    }
    Ordering.by(_.data)
  }
  def apply(singleValue: Double): PriorityChain = PriorityChain(Vector(singleValue))
  def apply(multiValues: Double*): PriorityChain = PriorityChain(multiValues.toVector)
}

trait UnitRequest[T <: WrapsUnit] {

  private val onDisposeActions    = ArrayBuffer.empty[OnClearAction]
  private var picker              = Option.empty[UnitWithJob[T] => PriorityChain]
  private var filter              = Option.empty[T => Boolean]
  private var autoCleanAfterTick  = true
  private var keepResourcesLocked = false
  def withFilter_!(rule: T => Boolean) = {
    filter = Some(rule)
    this
  }
  def withCherryPicker_!(rate: UnitWithJob[T] => PriorityChain) = {
    picker = Some(rate)
    this
  }
  def ratingFuntion = picker
  def priority: Priority
  def includesByType(unit: WrapsUnit): Boolean = typeOfRequestedUnit.isAssignableFrom(unit.getClass)
  def typeOfRequestedUnit: Class[_ <: T]
  def amount: Int
  def acceptableUntyped(unit: WrapsUnit) = typeOfRequestedUnit.isAssignableFrom(unit.getClass) &&
                                           acceptable(unit.asInstanceOf[T])
  def acceptable(unit: T) = filter.map(_.apply(unit)).getOrElse(true)
  def clearable = autoCleanAfterTick
  def dispose(): Unit = {
    onDisposeActions.foreach(_.onClear())
  }
  def doOnDispose_![X](u: => X) = {
    onDisposeActions += (() => u)
  }

  if (classOf[Building].isAssignableFrom(typeOfRequestedUnit)) {
    keepResourcesLocked_!()
  }

  def persistant_!(): Unit = {
    autoCleanAfterTick = false
  }
  def clearableInNextTick_!(): Unit = {
    autoCleanAfterTick = true
  }

  def forceUnlockOnDispose_!(): Unit = {
    keepResourcesLocked = false
  }

  def keepResourcesLocked_!(): Unit = {
    keepResourcesLocked = true
  }

  def unlocksResourcesOnDispose = !keepResourcesLocked

  override def toString = s"UnitRequest($typeOfRequestedUnit, $amount)"
}

case class AnyUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[_ <: T], amount: Int) extends UnitRequest[T] {
  override def priority: Priority = Priority.Default

}

case class AnyFactoryRequest[T <: UnitFactory, U <: Mobile](typeOfRequestedUnit: Class[_ <: T], amount: Int,
                                                            buildThis: Class[_ <: U]) extends UnitRequest[T] {
  override def acceptable(unit: T): Boolean = {
    super.acceptable(unit) && unit.canBuild(buildThis)
  }
  override def priority: Priority = Priority.Default
}

trait OnClearAction {
  def onClear(): Unit
}

case class BuildUnitRequest[T <: WrapsUnit](universe: Universe, typeOfRequestedUnit: Class[_ <: T], amount: Int,
                                            funding: ResourceApproval, override val priority: Priority,
                                            customBuildingPosition: AlternativeBuildingSpot,
                                            belongsTo: Option[ResourceArea] = None)
  extends UnitRequest[T] with HasFunding with HasUniverse {

  self =>

  def isAddon = classOf[Addon].isAssignableFrom(typeOfRequestedUnit)

  def isBuilding = classOf[Building].isAssignableFrom(typeOfRequestedUnit)

  def isMobile = classOf[Mobile].isAssignableFrom(typeOfRequestedUnit)

  if (customBuildingPosition.shouldUse) {
    assert(typeOfRequestedUnit.toUnitType.isBuilding)
  }

  override def proofForFunding = funding

  override def acceptable(unit: T): Boolean = false // refuse all that exist

  def customPosition = customBuildingPosition

  override def dispose(): Unit = {
    super.dispose()
    // for buildings, the resources need to be locked until the building process has begun
    if (unlocksResourcesOnDispose) {
      trace(s"Unlock during dispose: $self")
      unlock_!()
    }
  }
}

case class SpecificUnitRequest[T <: WrapsUnit](unit: T) extends UnitRequest[T] {
  override def typeOfRequestedUnit = unit.getClass
  override def amount: Int = 1
  override def priority: Priority = Priority.Default
}

trait RateCandidate[T <: WrapsUnit] {
  def giveRating(forThatOne: UnitWithJob[T]): PriorityChain
}

case class UnitJobRequests[T <: WrapsUnit : Manifest](requests: Seq[UnitRequest[T]], employer: Employer[T],
                                                      priority: Priority,
                                                      makeSureDependenciesCleared: Set[Class[_ <: WrapsUnit]] = Set
                                                                                                                .empty) {
  private val types = requests.map(_.typeOfRequestedUnit).toSet
  def canInterrupt(unit: UnitWithJob[_ <: WrapsUnit]) = {
    assert(priority > unit.priority, "Oops :(")
    // perfect solution: match every type against every type and use heavy logic to determine the result
    // reality: lazily add cases :D
    unit match {
      case i: Interruptable if i.interruptableNow => true
      case _ => false
    }
  }

  assert(requests.forall(_.clearable) || requests.forall(!_.clearable))
  def allRequiredTypes = requests.map(_.typeOfRequestedUnit).toSet ++ makeSureDependenciesCleared
  def priorityRule: Option[RateCandidate[T]] = {
    val picker = if (requests.size == 1) requests.head.ratingFuntion else None
    picker.map { nat => new RateCandidate[T] {
      def giveRating(forThatOne: UnitWithJob[T]): PriorityChain = nat(forThatOne)
    }
    }
  }

  def acceptOnly_!(only: T => Boolean) = {
    requests.foreach {_.withFilter_!(only)}
    this
  }

  def clearable = requests.forall(_.clearable)

  def wantsUnit(existingUnit: WrapsUnit) = types.exists(_.isAssignableFrom(existingUnit.getClass)) &&
                                           requests.exists(_.acceptableUntyped(existingUnit))

  def commonUnitType = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]

  def onClear(): Unit = requests.foreach(_.dispose())
}

object UnitJobRequests {
  def upgraderFor(upgrade: Upgrade, employer: Employer[Upgrader], priority: Priority = Priority.Upgrades) = {
    val actualClass = employer.race.techTree.upgraderFor(upgrade).asInstanceOf[Class[Upgrader]]
    val req = AnyUnitRequest(actualClass, 1)
    UnitJobRequests(req.toSeq, employer, priority).acceptOnly_!(!_.isDoingResearch)
  }

  def builderOf[T <: Mobile, F <: UnitFactory : Manifest](wantedType: Class[_ <: T],
                                                          employer: Employer[F],
                                                          priority: Priority = Priority.Default): UnitJobRequests[F] = {

    val actualClass = employer.universe.myRace.specialize(manifest[F].runtimeClass.asInstanceOf[Class[F]])
    val req = AnyFactoryRequest[F, T](actualClass, 1, wantedType)

    UnitJobRequests(req.toSeq, employer, priority)
  }

  def constructor[T <: WorkerUnit : Manifest](employer: Employer[T],
                                              priority: Priority = Priority.ConstructBuilding): UnitJobRequests[T] = {

    val actualClass = employer.universe.myRace.specialize(manifest[T].runtimeClass).asInstanceOf[Class[T]]
    val req = AnyUnitRequest(actualClass, 1).withCherryPicker_!(WorkerUnit.currentPriority)

    UnitJobRequests(req.toSeq, employer, priority)
  }

  def addonConstructor[T <: CanBuildAddons : Manifest](employer: Employer[T],
                                                       what: Class[_ <: Addon],
                                                       priority: Priority = Priority
                                                                            .ConstructBuilding) = {

    val actualClass = employer.universe.myRace.specialize(what)
    val mainType = employer.race.techTree.mainBuildingOf(what).asInstanceOf[Class[T]]
    val req = AnyUnitRequest(mainType, 1)
              .withFilter_!(e => !e.isBeingCreated && !e.hasAddonAttached && !e.isBuildingAddon)

    UnitJobRequests(req.toSeq, employer, priority, Set(what))
  }

  def idleOfType[T <: WrapsUnit : Manifest](employer: Employer[T], ofType: Class[_ <: T], amount: Int = 1,
                                            priority: Priority = Priority.Default) = {
    val realType = employer.universe.myRace.specialize(ofType)
    val req: AnyUnitRequest[T] = AnyUnitRequest(realType, amount)

    UnitJobRequests[T](req.toSeq, employer, priority)
  }

  def newOfType[T <: WrapsUnit : Manifest](universe: Universe, employer: Employer[T], ofType: Class[_ <: T],
                                           funding: ResourceApprovalSuccess, amount: Int = 1,
                                           priority: Priority = Priority.Default,
                                           customBuildingPosition: AlternativeBuildingSpot = AlternativeBuildingSpot
                                                                                             .useDefault,
                                           belongsTo: Option[ResourceArea] = None) = {
    val actualType = universe.myRace.specialize(ofType)
    val req: BuildUnitRequest[T] = BuildUnitRequest(universe, actualType, amount, funding, priority,
      customBuildingPosition, belongsTo)
    // this one needs to survive across ticks
    req.persistant_!()
    UnitJobRequests[T](req.toSeq, employer, priority)
  }
}

class SendOrdersToStarcraft(universe: Universe) extends AIModule[Controllable](universe) {
  override def ordersForTick: Traversable[UnitOrder] = {
    unitManager.allJobsByUnitType[Controllable].filterNot(_.hasFailed).flatMap { job =>
      job.ordersForThisTick
    }
  }
}

class JobReAssignments(universe: Universe) extends OrderlessAIModule[Controllable](universe) {
  override def onTick(): Unit = {
    unitManager.nextJobReorganisationRequest.foreach { optimizeMe =>
      trace(s"Trying to find better unit for job $optimizeMe")
      if (optimizeMe.hasToSwitchLater) {
        // try again later
        trace(s"Not found, try again later")
        unitManager.tryFindBetterEmployeeFor(optimizeMe)
      } else {
        def doTyped[T <: WrapsUnit : Manifest](old: CanAcceptUnitSwitch[T]) = {
          val uc = new UnitCollector(old.asRequest, universe).collect_!(List(old))
          uc match {
            case Some(replacement) if replacement.hasOneMember && replacement.onlyMember == optimizeMe.unit =>
              trace(s"Same unit chosen as best worker")
            case Some(replacement) if replacement.hasOneMember && replacement.onlyMember != optimizeMe.unit =>
              info(s"Replacement found: ${replacement.onlyMember}")
              //someone else might have already done this somewhere else
              val nobody = unitManager.Nobody
              if (unitManager.employerOf(optimizeMe.unit).contains(nobody)) {
                warn(s"Unit ${optimizeMe.unit} was already asigned to ${nobody}")
              } else {
                unitManager.assignJob_!(new BusyDoingNothing(optimizeMe.unit, nobody))
              }
              assert(!old.hasFailed)
              unitManager.assignJob_!(old.newFor(replacement.onlyMember))
            case _ =>
              if (optimizeMe.couldSwitchInTheFuture) {
                // try again later
                trace(s"Not found, try again later")
                unitManager.tryFindBetterEmployeeFor(optimizeMe)
              }
          }
        }
        doTyped(optimizeMe)
      }
    }
  }
}

