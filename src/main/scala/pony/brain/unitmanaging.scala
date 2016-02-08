package pony
package brain

import java.util.Comparator

import bwapi.Order
import pony.Orders.Stop
import pony.brain.UnitRequest.CherryPickers
import pony.brain.modules.AlternativeBuildingSpot

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class UnitManager(override val universe: Universe) extends HasUniverse {
  private val reorganizeJobQueue          = ListBuffer.empty[CanAcceptUnitSwitch[_ <: WrapsUnit]]
  private val unfulfilledRequestsThisTick = ArrayBuffer.empty[UnitJobRequest[_ <: WrapsUnit]]
  private val assignments                 = mutable.HashMap
                                            .empty[WrapsUnit, UnitWithJob[_ <: WrapsUnit]]
  private val allJobs                     = new JobsInTree
  private var unfulfilledRequestsLastTick = unfulfilledRequestsThisTick.toVector

  def employerOf(unit: WrapsUnit) = {
    assignments.get(unit).flatMap { job =>
      allJobs.allFlat.find { e =>
        val jobsOfEmployer = e._2
        jobsOfEmployer.contains(job)
      }
    }.map(_._1)
  }

  def hasJob(support: OrderHistorySupport) = assignments.contains(support)

  def allIdleMobiles = allJobsByType[BusyDoingNothing[Mobile]].filter(_.unit.isInstanceOf[Mobile])

  def allIdles = allJobsByType[BusyDoingNothing[WrapsUnit]]

  def allJobsWithReleaseableResources = allJobsWithPotentialFunding.filter(_.canReleaseResources)

  def allJobsWithPotentialFunding = assignments.values.collect { case f: JobHasFunding[_] => f }

  def existsOrPlanned(c: Class[_ <: WrapsUnit]) = {
    ownUnits.ownsByType(c) ||
    plannedToBuild.exists(e => c.isAssignableFrom(e.typeOfRequestedUnit)) ||
    plannedToTrain.exists(e => c.isAssignableFrom(e.typeOfRequestedUnit))
  }

  def plannedToTrain = allUnfulfilled.iterator.map(_.request)
                       .collect { case t: BuildUnitRequest[_] if t.isMobile => t }

  def existsAndDone(c: Class[_ <: WrapsUnit]) = {
    ownUnits.existsComplete(c)
  }

  def nextJobReorganisationRequest = {
    // clean at least one per tick
    val ret = reorganizeJobQueue.headOption
    if (reorganizeJobQueue.nonEmpty) {
      reorganizeJobQueue.remove(0)
    }
    trace(s"${reorganizeJobQueue.size} jobs left to optimize")
    ret
  }

  def tryFindBetterEmployeeFor[T <: WrapsUnit](anyJob: CanAcceptUnitSwitch[T]): Unit = {
    trace(s"Queued $anyJob for optimization")
    reorganizeJobQueue += anyJob
  }

  def plannedToBuildByType[T <: Building : Manifest]: Int = {
    val typeOfFactory = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]
    unfulfilledByTargetType(typeOfFactory).size
  }

  def plannedToBuildByClass(typeOfFactory: Class[_ <: Building]) = {
    unfulfilledByTargetType(typeOfFactory)
  }

  private def unfulfilledByTargetType[T <: WrapsUnit](targetType: Class[_ <: T]) = {
    allUnfulfilled.iterator.map(_.request).collect {
      case b: BuildUnitRequest[_] if b.typeOfRequestedUnit == targetType => b
    }
  }

  def allUnfulfilled = unfulfilledRequestsLastTick.toSet ++
                       unfulfilledRequestsThisTick.toSet

  def requestedConstructions[T <: Building : Manifest] = {
    val typeOfFactory = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]
    unfulfilledByTargetType(typeOfFactory)
  }

  def constructionsInProgress[T <: Building : Manifest]: Seq[ConstructBuilding[WorkerUnit, T]] = {
    constructionsInProgress(manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]])
  }

  def constructionsInProgress[T <: Building](typeOfBuilding: Class[_ <: T]):
  Seq[ConstructBuilding[WorkerUnit, T]] = {
    val byJob = allJobsByType[ConstructBuilding[WorkerUnit, Building]].collect {
      case cr: ConstructBuilding[WorkerUnit, Building] if typeOfBuilding
                                                          .isAssignableFrom(cr.typeOfBuilding) =>
        cr.asInstanceOf[ConstructBuilding[WorkerUnit, T]]
    }
    byJob
  }

  def jobsByType = assignments.values.toSeq.groupBy(_.getClass)

  def jobsOf[T <: WrapsUnit](emp: Employer[T]) = allJobs.allFlat.getOrElse(emp, Set.empty)
                                                 .asInstanceOf[collection.Set[UnitWithJob[T]]]

  def jobByUnitIdString(str: String) = assignments.find(_._1.unitIdText == str).map(_._2)

  def employers = allJobs.employers

  def plannedSupplyAdditions = {
    val byJob = allJobsByType[ConstructBuilding[WorkerUnit, Building]].collect {
      case cr: ConstructBuilding[WorkerUnit, Building] => cr.typeOfBuilding.toUnitType
                                                          .supplyProvided()
    }.sum
    val byUnfulfilledRequest = allUnfulfilled.map(_.request).collect {
      case b: BuildUnitRequest[_] => b.typeOfRequestedUnit.toUnitType.supplyProvided()
    }.sum
    byJob + byUnfulfilledRequest
  }

  def allJobsByType[T <: UnitWithJob[_] : Manifest] = {
    val wanted = manifest[T].runtimeClass
    assignments.valuesIterator.filter { job =>
      wanted.isAssignableFrom(job.getClass)
    }.map {_.asInstanceOf[T]}.toVector
  }

  def allJobsByUnitType[T <: WrapsUnit : Manifest] = selectJobs[T, UnitWithJob[T]](_ => true)

  def selectJobs[U <: WrapsUnit : Manifest, T <: UnitWithJob[U] : Manifest](f: T => Boolean) = {
    val wanted = manifest[U].runtimeClass
    assignments.valuesIterator.filter { job =>
      wanted.isInstance(job.unit) && f(job.asInstanceOf[T])
    }.map {_.asInstanceOf[T]}.toVector
  }

  def failedToProvideByType[T <: WrapsUnit : Manifest] = {
    val c = manifest[T].runtimeClass
    failedToProvideFlat.collect {
      case req: UnitRequest[_] if c.isAssignableFrom(req.typeOfRequestedUnit) => req
                                                                                 .asInstanceOf[UnitRequest[T]]
    }
  }

  def failedToProvideFlat = failedToProvide.map(_.request)

  def failedToProvide = unfulfilledRequestsLastTick.toSeq

  def jobOptOf[T <: WrapsUnit](unit: T) = assignments.get(unit).asInstanceOf[Option[UnitWithJob[T]]]

  def jobOf[T <: WrapsUnit](unit: T) = assignments(unit).asInstanceOf[UnitWithJob[T]]

  def tick(): Unit = {
    // do not pile these up, clear per tick - this is why unitmanagers tick must come last.
    val (clearable, keep) = unfulfilledRequestsLastTick.partition(_.clearable)
    clearable.foreach(_.onClear())
    unfulfilledRequestsLastTick = unfulfilledRequestsThisTick.toVector ++ keep
    unfulfilledRequestsThisTick.clear()

    //clean/update
    ownUnits.all.foreach(_.onTick_!())
    assignments.valuesIterator.foreach(_.onTick_!())
    enemies.all.foreach(_.onTick_!())
    val removeUs = {
      val done = assignments.filter { case (_, job) => job.isFinished }.values
      val failed = assignments.filter { case (_, job) => job.failedOrObsolete }.values

      trace(s"${failed.size}/${done.size} jobs failed/finished, putting units on the market again",
        failed.nonEmpty || done.nonEmpty)

      failed.foreach { failure =>
        info(s"FAIL! $failure")
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
        case cd: MaybeCanDie if !cd.isDead =>
          val newJob = new BusyDoingNothing(cd, Nobody)
          assignJob_!(newJob)
        case cd: MaybeCanDie if cd.isDead =>
          assignments.remove(cd)
          allJobs.removeBinding(job)
        case res: MineralPatch =>
          assignments.remove(res)
          allJobs.removeBinding(job)
        case _ =>
      }
      job.onFinishOrFail()
    }

    def initialJobOf[T <: WrapsUnit](unit: T) = {
      if (unit.isBeingCreated) {
        Some(unit match {
          case b: Building =>
            new BusyBeingContructed(unit, Constructor)
          case m: Mobile =>
            new BusyBeingTrained(unit, Trainer)
          case _ => throw new UnsupportedOperationException(s"Check this: $unit")
        })
      } else if (!unit.isInstanceOf[Irrelevant]) {
        Some(new BusyDoingNothing(unit, Nobody))
      } else {
        None
      }
    }

    val myOwn = universe.world
                .ownUnits
                .inFaction
                .filterNot(assignments.contains)
                .flatMap(e => initialJobOf(e).toList)
                .toSeq
    info(s"Found ${myOwn.size} new units of player", myOwn.nonEmpty)

    myOwn.foreach(assignJob_!)
    assignments ++= myOwn.map(e => e.unit -> e)

    val registerUs = universe.world
                     .ownUnits
                     .all
                     .filterNot(assignments.contains)
                     .flatMap(e => initialJobOf(e).toList)
                     .toSeq
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
      val oldAssignment = allJobs.findEmployerBy(oldJob)
      assert(oldAssignment.isDefined)
      oldAssignment.foreach { oldEmployer =>
        trace(s"Old job assignment was: $oldJob, employed by $oldEmployer")
        assert(oldEmployer == oldJob.employer)
        allJobs.removeBinding(oldJob)
        val oldEmployerTyped = oldEmployer.asInstanceOf[Employer[T]]
        oldEmployerTyped.hiredBySomeoneMoreImportant_!(newUnit)
      }
    }

    assignments.put(newUnit, newJob)
    allJobs.addBinding(employer, newJob)
  }

  def allRequirementsFulfilled[T <: WrapsUnit](c: Class[_ <: T]) = {
    val (m, i, p) = findMissingRequirements[T](collection.immutable.Set(c))
    m.isEmpty && i.isEmpty && p.isEmpty
  }

  def requirementsQueuedToBuild[T <: WrapsUnit](c: Class[_ <: T]) = {
    val (_, i, p) = findMissingRequirements[T](collection.immutable.Set(c))
    i.nonEmpty && p.isEmpty
  }

  private def findMissingRequirements[T <: WrapsUnit](c: Set[Class[_ <: T]]) = {
    val mustHave = c.flatMap(race.techTree.requiredFor)
    val i = mutable.Set.empty[Class[_ <: Building]]
    val m = mutable.Set.empty[Class[_ <: Building]]
    val p = mutable.Set.empty[Class[_ <: Building]]
    val missing = {
      mustHave.foreach { dependency =>
        val complete = ownUnits.existsComplete(dependency)
        if (!complete) {
          val incomplete = ownUnits.existsIncomplete(dependency)
          if (incomplete) {
            i += dependency
          } else {
            val planned = unitManager.plannedToBuild(dependency)
            if (planned) {
              p += dependency
            } else {
              m += dependency
            }
          }
        }
      }
    }
    (m.toSet, i.toSet, p.toSet)
  }

  def plannedToBuild(c: Class[_ <: Building]): Boolean = plannedToBuild
                                                         .exists(e => c.isAssignableFrom(
                                                           e.typeOfRequestedUnit))

  def plannedToBuild = allUnfulfilled.map(_.request)
                       .collect { case b: BuildUnitRequest[_] if b.isBuilding => b }

  def requestWithoutTracking[T <: WrapsUnit : Manifest](req: UnitJobRequest[T],
                                                        forceInclude: Set[UnitWithJob[T]] = Set
                                                                                            .empty[UnitWithJob[T]]) = {
    collectCandidates(req, forceInclude).map(_.teamAsCanHireInfo.details).getOrElse(Set.empty)
  }

  private def collectCandidates[T <: WrapsUnit : Manifest](req: UnitJobRequest[T],
                                                           forceInclude: Set[UnitWithJob[T]] = Set
                                                                                               .empty[UnitWithJob[T]]) = {
    new UnitCollector[T](req, universe).collect_!(forceInclude)
  }

  def request[T <: WrapsUnit : Manifest](req: UnitJobRequest[T],
                                         buildIfNoneAvailable: Boolean = true) = {
    trace(s"${req.employer} requested ${req.request.toString}")
    val (missing, incomplete, planned) = findMissingRequirements(req.allRequiredTypes)
    val result = if (missing.isEmpty && incomplete.isEmpty && planned.isEmpty) {
      val hr = collectCandidates(req)
      hr match {
        case None =>
          if (buildIfNoneAvailable) unfulfilledRequestsThisTick += req
          new FailedPreHiringResult[T]
        case Some(team) if !team.complete =>
          trace(s"Partially successful hiring request: $team")
          if (buildIfNoneAvailable) unfulfilledRequestsThisTick += team.missingAsRequest
          if (team.hasOneMember)
            new PartialPreHiringResult(team.teamAsCanHireInfo) with ExactlyOneSuccess[T]
          else
            new PartialPreHiringResult(team.teamAsCanHireInfo)
        case Some(team) =>
          trace(s"Successful hiring request: $team")
          if (team.hasOneMember) {
            new SuccessfulPreHiringResult(team.teamAsCanHireInfo) with ExactlyOneSuccess[T]
          } else {
            new SuccessfulPreHiringResult(team.teamAsCanHireInfo)
          }
      }
    } else {
      new MissingRequirementResult[T](missing, incomplete, planned)
    }

    trace(s"Result of request: $result")
    result
  }

  def allOfEmployerAndType[T <: WrapsUnit](employer: Employer[T], unitType: Class[_ <: T]) = {
    allJobs.jobsOf(employer, unitType)
  }

  def allNotOfEmployerButType[T <: WrapsUnit](employer: Employer[T], unitType: Class[_ <: T]) = {
    allJobs.employers.asInstanceOf[collection.Set[Employer[T]]]
    .iterator
    .filter(_ != employer)
    .flatMap { e =>
      allJobs.jobsOf(e, unitType)
    }
  }

  def allOfEmployer[T <: WrapsUnit](employer: Employer[T]) = allJobs.allOfEmployer(employer)

  def allNotOfEmployer[T <: WrapsUnit](employer: Employer[T]) = allJobs.allNotOfEmployer(employer)

  def nobody = Nobody

  case class JobIndex[T <: WrapsUnit](e: Employer[T], c: Class[_ <: T]) {
    def fitsTo[X <: WrapsUnit](e: Employer[X], u: X) = this.e == e && c.isInstance(u)

    def fitsTo[X <: WrapsUnit](j: UnitWithJob[X]) = e == j.employer && c.isInstance(j.unit)
  }

  class JobsInTree {
    private val flat       = multiMap[Employer[_ <: WrapsUnit], UnitWithJob[_ <: WrapsUnit]]
    private val indexBy    = mutable.HashSet.empty[JobIndex[_ <: WrapsUnit]]
    private val byEmployer = mutable.HashMap
                             .empty[JobIndex[_ <: WrapsUnit], JobsByClass[_ <: WrapsUnit]]

    def allNotOfEmployer[T <: WrapsUnit](employer: Employer[T]) = {
      allFlat.filter(_._1 != employer).flatMap(_._2)
    }

    def allFlat = flat

    def jobsOf[T <: WrapsUnit](employer: Employer[T], unitType: Class[_ <: T]) = {
      val key = JobIndex(employer, unitType)

      val node = getTyped[T](key)

      if (!indexBy(key)) {
        indexBy += key
        allOfEmployer(employer)
        .filter(e => unitType.isInstance(e.unit))
        .foreach { j =>
          node.add(j)
        }
      }

      node.all
    }

    def allOfEmployer[T <: WrapsUnit](e: Employer[T]) = {
      allFlat.getOrElse(e, Set.empty).asInstanceOf[collection.Set[UnitWithJob[T]]]
    }

    def getTyped[T <: WrapsUnit](c: JobIndex[_ <: WrapsUnit]): JobsByClass[T] = {
      byEmployer.getOrElseUpdate(c, new JobsByClass[T]).asInstanceOf[JobsByClass[T]]
    }

    def addBinding[T <: WrapsUnit](employer: Employer[T], newJob: UnitWithJob[T]): Unit = {
      flat.addBinding(employer, newJob)
      indexBy.iterator.filter(_.fitsTo(employer, newJob.unit)).foreach { c =>
        val node = getTyped[T](c)
        node.add(newJob)
      }
    }

    def findEmployerBy[T <: WrapsUnit](oldJob: UnitWithJob[T]) = {
      flat.find(_._2(oldJob)).map(_._1.asInstanceOf[Employer[T]])
    }

    def removeBinding[T <: WrapsUnit](job: UnitWithJob[T]) = {
      flat.removeBinding(job.employer, job)
      indexBy.iterator.filter(_.fitsTo(job)).foreach { c =>
        val node = getTyped[T](c)
        node.remove(job)
      }
    }

    def employers = flat.keySet

    class JobsByClass[T <: WrapsUnit] {
      private val jobs = mutable.HashSet.empty[UnitWithJob[T]]

      def all = jobs

      def add(newJob: UnitWithJob[T]) = {
        assert(!jobs(newJob))
        jobs += newJob
      }

      def remove(unit: UnitWithJob[T]) = {
        assert(jobs(unit))
        jobs -= unit
      }
    }

  }

  case object Nobody extends Employer[WrapsUnit](universe)

  case object Trainer extends Employer[WrapsUnit](universe)

  case object Constructor extends Employer[WrapsUnit](universe)

}

trait PreHiringResult[T <: WrapsUnit] {
  def hasAnyMissingRequirements = notExistingMissingRequiments.nonEmpty ||
                                  inProgressMissingRequirements.nonEmpty ||
                                  plannedMissingRequirements.nonEmpty

  def notExistingMissingRequiments: Set[Class[_ <: Building]] = Set.empty
  def inProgressMissingRequirements: Set[Class[_ <: Building]] = Set.empty
  def plannedMissingRequirements: Set[Class[_ <: Building]] = Set.empty
  def success: Boolean
  def canHire: CanHireInfo[T]
  def units = canHire.details

  override def toString = s"HiringResult($success, $canHire)"

  def mapOne[X](todo: T => X) = {
    var result = Option.empty[X]
    ifOne { in =>
      val ret = todo(in)
      result = Some(ret)
    }
    result
  }

  def ifOne[X](todo: T => X) = this match {
    case one: ExactlyOneSuccess[T] => todo(one.onlyOne)
    case _ =>
  }

  def ifNotZero[X](todo: Seq[T] => X): Unit = ifNotZero(todo, {})

  def ifNotZero[X](todo: Seq[T] => X, orElse: X) = this match {
    case many: AtLeastOneSuccess[T] =>
      val canHireThese = many.canHire.details.toList
      todo(canHireThese)
    case _ => orElse
  }
}

class FailedPreHiringResult[T <: WrapsUnit] extends PreHiringResult[T] {
  def success = false

  def canHire = CanHireInfo.empty
}

class MissingRequirementResult[T <: WrapsUnit](needs: Set[Class[_ <: Building]],
                                               incomplete: Set[Class[_ <: Building]],
                                               planned: Set[Class[_ <: Building]])
  extends PreHiringResult[T] {
  def success = false

  def canHire = CanHireInfo.empty

  override def notExistingMissingRequiments = needs

  override def inProgressMissingRequirements = incomplete

  override def plannedMissingRequirements = planned
}

trait AtLeastOneSuccess[T <: WrapsUnit] extends PreHiringResult[T] {
  def one = canHire.details.head
}

trait ExactlyOneSuccess[T <: WrapsUnit] extends AtLeastOneSuccess[T] {
  def onlyOne = {
    assert(canHire.details.size == 1)
    canHire.details.head
  }
}

case class CanHireInfo[T <: WrapsUnit](request: Option[UnitJobRequest[T]], details: Set[T])

object CanHireInfo {
  def empty[T <: WrapsUnit] = CanHireInfo[T](None, Set.empty)
}

class SuccessfulPreHiringResult[T <: WrapsUnit](override val canHire: CanHireInfo[T])
  extends PreHiringResult[T] with AtLeastOneSuccess[T] {
  def success = true
}

class PartialPreHiringResult[T <: WrapsUnit](override val canHire: CanHireInfo[T])
  extends PreHiringResult[T] with AtLeastOneSuccess[T] {
  def success = false
}

class UnitCollector[T <: WrapsUnit : Manifest](req: UnitJobRequest[T], override val universe:
Universe)
  extends HasUniverse {

  private val hired              = mutable.HashSet.empty[T]
  private var remainingOpenSpots = req.request.amount

  def onlyMember = {
    assert(hasOneMember)
    hired.head
  }

  def hasOneMember = hired.size == 1

  def collect_!(includeCandidates: Set[UnitWithJob[T]] = Set
                                                         .empty[UnitWithJob[T]]):
  Option[UnitCollector[T]] = {
    val um = unitManager
    val available = {
      val potential = {
        val targetType = req.requestedUnitType

        def allWithType = um.allOfEmployerAndType(um.Nobody, req.requestedUnitType).iterator ++
                          um.allNotOfEmployerButType(um.Nobody, req.requestedUnitType)

        val defaultSuggestions = allWithType.filter(_.unit.isInGame)
                                 .filter {
                                   _.unit match {
                                     case a: AutoPilot => a.isManuallyControlled
                                     case _ => true
                                   }
                                 }
                                 .filter(_.priority < req.priority)
                                 .filter(interrupts)
                                 .filter(requests)
        val withExplicitCandidates = defaultSuggestions ++ includeCandidates
        withExplicitCandidates.map(typed).toVector
      }
      priorityRule.fold(potential) { rule =>
        val prepared = potential.map(e => e -> rule.giveRating(e))
        prepared.sortBy(_._2).map(_._1)
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
    if (remainingOpenSpots > 0 && req.request.includesByType(unit)) {
      remainingOpenSpots -= 1
      // we know the type because we asked req before
      hired += unit.asInstanceOf[T]
    }
  }

  def complete = remainingOpenSpots == 0

  def requests(unit: UnitWithJob[_ <: WrapsUnit]) = {
    req.wantsUnit(unit.unit)
  }

  def interrupts(unit: UnitWithJob[_ <: WrapsUnit]) = {
    req.canInterrupt(unit)
  }

  def hasPriorityRule = priorityRule.isDefined

  def priorityRule = req.priorityRule

  def missingAsRequest: UnitJobRequest[T] = {
    val typesAndAmounts =
      BuildUnitRequest[T](universe, req.request.typeOfRequestedUnit, remainingOpenSpots,
        ResourceApprovalFail,
        Priority.Default,
        AlternativeBuildingSpot.useDefault)

    UnitJobRequest[T](typesAndAmounts, req.employer, req.priority)
  }

  override def toString: String = s"Collected: $teamAsCanHireInfo"

  def teamAsCanHireInfo = CanHireInfo(Some(req), hired.toSet)
}

// TODO check if this class really has a purpose
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

trait CanAcceptSwitchAndHasFunding[T <: WrapsUnit]
  extends JobHasFunding[T] with CanAcceptUnitSwitch[T] {
  override def onStealUnit(): Unit = {
    super.onStealUnit()
  }
}

trait CanAcceptUnitSwitch[T <: WrapsUnit] extends UnitWithJob[T] {
  def stillWantsOptimization = !failedOrObsolete
  def copyOfJobForNewUnit(replacement: T): UnitWithJob[T]
  def asRequest: UnitJobRequest[T]
  def canSwitchNow: Boolean
  def couldSwitchInTheFuture: Boolean
  def hasToSwitchLater = !canSwitchNow && couldSwitchInTheFuture
}

trait Interruptable[T <: WrapsUnit] extends UnitWithJob[T] {
  def interruptableNow = unit match {
    case gu: GroundUnit => gu.onGround
    case _ => true
  }
}

object JobCounter {
  private var jobs = 0

  def next() = {
    jobs += 1
    jobs
  }
}

abstract class UnitWithJob[T <: WrapsUnit](val employer: Employer[T], val unit: T,
                                           val priority: Priority) extends JobOrSubJob[T] {

  override val universe      = employer.universe
  private  val myJobId       = JobCounter.next()
  private  val realStartTick = currentTick
  private  val listeners     = ArrayBuffer.empty[JobFinishedListener[T]]
  private  var forceFail     = false
  private  var obsolete      = false
  private  var startTick     = currentTick

  unit match {
    case cd: MaybeCanDie if cd.isDead => fail_!()
    case _ =>
  }
  private var noCommandsForTicks            = 0
  private var dead                          = false
  private var lastOrder: Seq[UnitOrder]     = Nil
  private var lastNonInterruptedOrderGiven  = Option.empty[Int]
  private var firstNonInterruptedOrderGiven = Option.empty[Int]

  ownUnits.registerKill_!(OnKillListener.on(unit, () => {
    trace(s"Unit $unit died, aborting $this")
    dead = true
  }))

  def resetTimer_!(): Unit = {
    startTick = 0
  }

  def hasNotYetSpendResources: Boolean = true

  override def onTick_!(): Unit = {
    super.onTick_!()
  }

  def onStealUnit() = {}

  def shortDebugString: String

  def ageSinceLastReset = currentTick - startTick

  def age = currentTick - realStartTick

  def isIdle: Boolean = false

  def lastIssuedOrder = lastOrder

  def wasInterceptedLastTick = ageSinceLastNonInterceptedOrder.fold(true)(_ > 1)

  def ageSinceLastNonInterceptedOrder = lastNonInterruptedOrderGiven.map(currentTick - _)

  def ageSinceFirstNonInterceptedOrder = firstNonInterruptedOrderGiven.map(currentTick - _)

  def ordersForThisTick = {
    if (failedOrObsolete) {
      Nil
    } else {
      if (noCommandsForTicks > 0) {
        noCommandsForTicks -= 1
        Nil
      } else {

        noCommandsForTicks = everyNth
        var sourceIsOriginal = false
        def injectedOrMine = {
          val maybe = higherPriorityOrder
          if (maybe.isEmpty) {
            sourceIsOriginal = true
            ordersForTick
          } else
            maybe
        }
        val nextOrder = injectedOrMine

        if (sourceIsOriginal) {
          lastNonInterruptedOrderGiven = Some(currentTick)
          firstNonInterruptedOrderGiven.forNone {
            firstNonInterruptedOrderGiven = lastNonInterruptedOrderGiven
          }
        }

        val shouldRepeat = !nextOrder.exists(_.forceRepetition) &&
                           omitRepeatedOrders &&
                           nextOrder == lastOrder &&
                           !unit.isDoingNothing
        if (shouldRepeat) {
          Nil
        } else {
          lastOrder = nextOrder
          nextOrder
        }
      }
    }
  }

  protected def omitRepeatedOrders = false

  def everyNth = 0

  def noCommandsForTicks_!(n: Int): Unit = {
    noCommandsForTicks = n
  }

  override def toString: String = s"[J#$myJobId] ${getClass.className} of $unit of $employer"

  def isFinished: Boolean

  def onFinishOrFail(): Unit = {
    listeners.foreach(_.onFinishOrFail(failedOrObsolete))
  }

  def failedOrObsolete = dead || forceFail || obsolete || jobHasFailedWithoutDeath

  private val inactive = Set(Order.PlayerGuard)

  def jobHasFailedWithoutDeath: Boolean = {
    unit match {
      case ohs: OrderHistorySupport => ohs.unitHistory.take(24).forall(e => inactive(e.order))
      case _ => false
    }
  }

  def listen_!(listener: JobFinishedListener[T]): Unit = listeners += listener

  def markObsolete_!(): Unit = {
    obsolete = true
  }

  def fail_!() = {
    forceFail = true
  }

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
  private var explicitlyUnlocked         = false
  private var autoUnlocked               = false
  private var unlockedDebug        : Any = null
  private var unlockedManuallyDebug: Any = null
  private var doNotUnlock                = false

  def stopManagingResource_!(): Unit = {
    trace(s"$this is no longer taking car of $proofForFunding")
    doNotUnlock = true
  }
  def proofForFunding: ResourceApproval
  def resources: ResourceManager
  def stillLocksResources = !explicitlyUnlocked && !autoUnlocked
  def notifyResourcesDisapproved_!(): Unit = {
    trace(s"Resources of $this just got disapproved")
    unlockManually_!()
  }

  def unlockManually_!(): Unit = {
    if (doNotUnlock) {
      trace(s"Supposed to unlock resources $proofForFunding of $this, but marked as noop")
    } else {
      assert(!explicitlyUnlocked, s"Don't do that twice")
      assert(!autoUnlocked, s"Too slow")
      trace(s"Manually unlocking $proofForFunding of $this")
      unlock_!()
      explicitlyUnlocked = true
      unlockedManuallyDebug = Thread.currentThread().getStackTrace
    }
  }

  def unlock_!(): Unit = {
    if (!doNotUnlock) {
      if (!explicitlyUnlocked) {
        assert(!autoUnlocked, s"Already unlocked: $this")
        proofForFunding match {
          case suc: ResourceApprovalSuccess =>
            autoUnlocked = true
            trace(s"Unlocking funds of $this : $proofForFunding")
            resources.unlock_!(suc)
            unlockedDebug = Thread.currentThread().getStackTrace
          case _ =>
        }
      } else {
        trace(s"Not unlocking $proofForFunding of $this automatically, someone already did that")
      }
    } else {
      trace(s"Tried to unlock resources $proofForFunding of $this, but marked as noop")
    }
  }
}

trait JobHasFunding[T <: WrapsUnit] extends UnitWithJob[T] with HasUniverse with HasFunding {
  self =>

  assert(proofForFunding.isSuccess, s"Problem, check $this")

  resources.informUsage(proofForFunding, this)

  def canReleaseResources = ageSinceLastReset == 0 && stillLocksResources

  override def notifyResourcesDisapproved_!(): Unit = {
    super.notifyResourcesDisapproved_!()
    fail_!()
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

class TrainUnit[F <: UnitFactory, T <: Mobile](factory: F, trainType: Class[_ <: T], employer:
Employer[F],
                                               funding: ResourceApprovalSuccess)
  extends UnitWithJob[F](employer, factory, Priority.Default) with JobHasFunding[F] with
          IssueOrderNTimes[F] with
          CreatesUnit[F] {

  private val patience = 20

  private var startedToProduce = false

  override def proofForFunding = funding

  override def getOrder: Seq[UnitOrder] = {
    assert(!failedOrObsolete, s"$this has failed, but is still asked for its order")
    info(s"Training $trainType")
    Orders.Train(unit, trainType).toSeq
  }

  override def onTick_!(): Unit = {
    super.onTick_!()

    if (!startedToProduce && ageSinceLastReset > patience && factory.isProducing) {
      startedToProduce = true
      trace(s"Job $this started to produce ${trainType.className}")
      unlockManually_!()
    }
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    ageSinceLastReset > patience + 5 && !startedToProduce
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
  extends UnitWithJob[W](employer, basis, Priority.Addon) with JobHasFunding[W] with
          CreatesUnit[W] with
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

  override def onTick_!(): Unit = {
    super.onTick_!()
    assert(failedOrObsolete || resources.hasStillLocked(proofForFunding),
      s"Someone stole $proofForFunding from $this")
    if (!startedConstruction) {
      startedConstruction = basis.isBuildingAddon
    }
    if (startedConstruction && !stoppedConstruction) {
      val addon = basis.hasCompleteAddon
      stoppedConstruction = addon
    }
  }

  override def proofForFunding = funding

  override def onFinishOrFail(): Unit = {
    super.onFinishOrFail()
    universe.ownUnits.allAddons.find(e => basis.positionedNextTo(e)).foreach { e =>
      basis.notifyAttach_!(e)
    }
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    def myFail = ageSinceLastReset > 50 && !startedConstruction
    super.jobHasFailedWithoutDeath || myFail
  }

  override def getOrder = Orders.ConstructAddon(basis, builtWhat).toSeq
}

class ResearchUpgrade[U <: Upgrader](employer: Employer[U],
                                     basis: U,
                                     what: Upgrade,
                                     funding: ResourceApproval)
  extends UnitWithJob[U](employer, basis, Priority.Upgrades) with JobHasFunding[U] with
          IssueOrderNTimes[U] {

  private var startedResearch     = false
  private var stoppedResearch     = false
  private var stoppedAtTick       = Option.empty[Int]
  private var gameConfirmsUpgrade = false

  override def shortDebugString: String = s"Research $what"

  override def isFinished: Boolean = {
    startedResearch && stoppedResearch && gameConfirmsUpgrade
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    super.jobHasFailedWithoutDeath || (ageSinceLastReset > 10 && !startedResearch) || {
      stoppedResearch && !gameConfirmsUpgrade && currentTick - stoppedAtTick.get > 24
    }
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (!startedResearch && basis.isDoingResearch) {
      startedResearch = true
    }
    if (startedResearch && !basis.isDoingResearch) {
      stoppedResearch = true
      stoppedAtTick = currentTick.toSome
    }
    if (stoppedResearch && !gameConfirmsUpgrade) {
      if (isResearchedInRealGame) {
        gameConfirmsUpgrade = true
      }
    }
  }

  private def isResearchedInRealGame = {
    what.nativeType.fold(
      u => {
        val actual = universe.world.nativeGame.self().getUpgradeLevel(u)
        val expected = universe.upgrades.upgradeLevelOf(u)
        actual == expected + 1
      },
      t => universe.upgrades.isTechResearchInNativeGame(t))

  }

  override def getOrder =
    Orders.Research(basis, what).toSeq

  override def proofForFunding = funding

}

trait JobOrSubJob[+T <: WrapsUnit] extends HasUniverse {
  def unit: T

  def renderDebug(renderer: Renderer) = {}

  protected def higherPriorityOrder = Seq.empty[UnitOrder]

}

trait PathfindingSupport[T <: Mobile] extends JobOrSubJob[T] {

  private val needsPath = oncePer(Primes.prime23) {
    val target = pathTargetPosition
    def areaOfTarget = target.flatMap(mapLayers.rawWalkableMap.areaOf)
    def areaOfUnit = unit.currentArea


    target match {
      case Some(where) =>
        val far = unit.currentTile.distanceToIsMore(where, 15)
        far && (unit match {
          case g: GroundUnit if g.onGround && areaOfTarget == areaOfUnit &&
                                areaOfTarget.isDefined =>
            !mapLayers.rawWalkableMap.connectedByLine(unit.currentTile, where)
          case a: AirUnit => true
          case _ => false
        })
      case None =>
        false
    }
  }
  private var myPath    = BWFuture.none[MigrationPath]

  override def renderDebug(renderer: Renderer) = {
    super.renderDebug(renderer)
    myPath.ifDone(_.foreach(_.renderDebug(renderer)))

  }

  override def higherPriorityOrder = {
    def newPathRequired(where: MapTilePosition): Unit = {
      trace(s"Unit $unit needs paths to $where")
      val pf = pathfinders.safeFor(unit)
      val task = pf.findPath(unit.currentTile, where).imap(_.toMigration)
      myPath = task
    }
    // must return noop instead of nil to cause a waiting behaviour
    def noopFallback: List[UnitOrder] = {
      if (waitForPath) {
        Orders.NoUpdate(unit).toList
      } else {
        Nil
      }
    }
    val myOrder = {
      if (needsPath.get) {
        pathTargetPosition.map { where =>
          if (myPath.isDone && myPath.assumeDoneAndGet.isEmpty) {
            newPathRequired(where)
          }

          myPath.foldOpt(noopFallback) { mig =>
            val outdated = mig.originalDestination.distanceToIsMore(where, 3)
            if (outdated) {
              newPathRequired(where)
              Nil
            } else {
              mig.nextPositionFor(unit)
              .map(Orders.MoveToTile(unit, _))
              .toList
            }
          }
        }.getOrElse(noopFallback)
      } else {
        myPath = BWFuture.none
        Nil
      }
    }
    if (myOrder.isEmpty) super.higherPriorityOrder else myOrder
  }

  protected def waitForPath: Boolean = true

  protected def pathTargetPosition: Option[MapTilePosition]
}

trait FerrySupport[T <: GroundUnit] extends JobOrSubJob[T] {

  override def higherPriorityOrder: Seq[UnitOrder] = {
    val where = unit.currentTile
    val target = ferryDropTarget

    val myOrder = target.map { to =>
      val needsFerry = !unit.currentArea.exists(_.free(to))
      if (needsFerry && mapLayers.rawWalkableMap.free(to)) {
        ferryManager.requestFerry_!(unit, to) match {
          case Some(plan) if unit.onGround =>
            Orders.BoardFerry(unit, plan.ferry).toList
          case _ if unit.loaded =>
            // do nothing while in transporter
            Orders.NoUpdate(unit).toList
          case None =>
            //go there while waiting for ferry
            Orders.MoveToTile(unit, to).toList
        }
      } else Nil
    }.getOrElse(Nil)
    if (myOrder.isEmpty) super.higherPriorityOrder else myOrder
  }

  protected def ferryDropTarget: Option[MapTilePosition]
}

class ConstructBuilding[W <: WorkerUnit : Manifest, B <: Building](worker: W,
                                                                   buildingType: Class[_ <: B],
                                                                   employer: Employer[W],
                                                                   val buildWhere: MapTilePosition,
                                                                   funding: ResourceApprovalSuccess,
                                                                   val belongsTo:
                                                                   Option[ResourceArea] = None)
  extends UnitWithJob[W](employer, worker, Priority.ConstructBuilding)
          with JobHasFunding[W]
          with CreatesUnit[W]
          with CanAcceptUnitSwitch[W]
          with FerrySupport[W]
          with CanAcceptSwitchAndHasFunding[W]
          with PathfindingSupport[W]
          with IssueOrderNTimes[W] {
  self =>

  assert(universe.mapLayers.rawWalkableMap.insideBounds(buildWhere),
    s"Target building spot is outside of map: $buildWhere, check $self")

  val area = {
    val unitType = buildingType.toUnitType
    val size = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    Area(buildWhere, size)
  }
  private val alternativeWorkers = {
    class Input {
      val target     = buildWhere
      val pathfinder = pathfinders.groundSafe
      val candidates = unitManager.ownUnits.allByType[W].map { w =>
        w.currentTile
      }
    }

    FutureIterator.feed(new Input).produceAsyncLater { in =>
      val paths = in.candidates.flatMap { where =>
        in.pathfinder.findSimplePathNow(where, in.target, tryFixPath = true)
      }
      val data = new ClosestPaths(paths)
      in.candidates.foreach { pos =>
        mapLayers.rawWalkableMap.spiralAround(pos, 5).foreach { precalcThis =>
          data.closestPathFrom(precalcThis)
        }
      }
      data
    }
  }
  private val isMainBuilding     = classOf[MainBuilding].isAssignableFrom(buildingType)

  listen_!((failed: Boolean) => {
    mapLayers.unblockBuilding_!(area)
  })

  assert(
    resources.detailedLocks.exists(e => e.whatFor == buildingType && e.reqs.sum == funding.sum),
    s"Something is wrong, check $this, it is supposed to have ${
      funding.sum
    } funding, but the resource manager only has\n ${
      resources.detailedLocks.mkString("\n")
    }\nlocked")
  private var startedMovingToSite       = false
  private var startedActualConstruction = false
  private var finishedConstruction      = false
  private var resourcesUnlocked         = false
  private var constructs                = Option.empty[Building]

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (unit.gotUnloaded) resetTimer_!()
    if (startedActualConstruction && !resourcesUnlocked && constructs.isDefined) {
      trace(s"Construction job $this no longer needs to lock resources")
      unlockManually_!()
      resourcesUnlocked = true
      mapLayers.unblockBuilding_!(area)
    }

    if (stillWantsOptimization) {
      // regularly try to optimize
      ifNth(Primes.prime43) {
        alternativeWorkers.prepareNextIfDone()
      }
      if (alternativeWorkers.hasResult) {
        unitManager.tryFindBetterEmployeeFor(this)
      }
    }
  }

  override def stillWantsOptimization = canSwitchNow &&
                                        buildWhere.distanceToIsMore(unit.currentTile, 3) &&
                                        stillLocksResources &&
                                        !failedOrObsolete &&
                                        unit.onGround

  override def canSwitchNow = {
    !startedActualConstruction
  }

  override def getOrder: Seq[UnitOrder] = {
    Orders.ConstructBuilding(worker, buildingType, buildWhere).toSeq
  }

  override def shortDebugString: String = s"Build ${buildingType.className}"

  override def proofForFunding = funding

  override def couldSwitchInTheFuture = !startedActualConstruction

  override def jobHasFailedWithoutDeath: Boolean = {
    if (unit.onGround) {
      val byState = !worker.isInConstructionProcess &&
                    ageSinceFirstNonInterceptedOrder.getOrElse(0) > times + 10 &&
                    !isFinished
      def byMap = !mapLayers.blockedByBuildingTiles.free(area) && constructs.isEmpty
      val fail = byState || byMap
      warn(s"Construction of ${typeOfBuilding.className} failed, worker $worker didn't manange",
        fail)
      fail
    } else {
      false
    }
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
          fail_!()
        }
      }
      trace(s"Worker $worker started to build $buildingType", startedActualConstruction)
    } else if (!finishedConstruction) {
      assert(constructs.isDefined)
      finishedConstruction = !worker.isInConstructionProcess
      trace(s"Worker $worker finished to build $buildingType", finishedConstruction)
    }
    ageSinceLastReset > times + 10 && startedMovingToSite && finishedConstruction
  }

  override def times = 50

  def building = constructs

  override def asRequest: UnitJobRequest[W] = {
    val picker = alternativeWorkers.mostRecent match {
      case Some(data) =>
        CherryPickers.cherryPickWorkerByDistance[W](area.centerTile) { from =>
          data.closestPathFrom(from)
        }
      case None =>
        CherryPickers.cherryPickWorkerByDistance[W](area.centerTile)()
    }

    UnitJobRequest.constructor[W](employer).withRequest(_.withCherryPicker_!(picker))
  }

  override def copyOfJobForNewUnit(replacement: W) = {
    assert(!failedOrObsolete)
    stopManagingResource_!()
    markObsolete_!()
    new ConstructBuilding(replacement, buildingType, employer, buildWhere, funding, belongsTo)
  }

  override protected def pathTargetPosition = {
    buildWhere.toSome
  }

  override protected def ferryDropTarget = area.centerTile.toSome

  private class ClosestPaths(source: Traversable[Path]) {
    private val cache = mutable.HashMap.empty[MapTilePosition, Double]

    def closestPathFrom(here: MapTilePosition) = {
      cache.getOrElseUpdate(here, {
        val candidates = source.map { path =>
          if (path.waypoints.nonEmpty) {
            path -> path.distanceToFinalTargetViaPath(here).getOr(s"Should never happen")
          } else {
            path -> 0.0
          }
        }
        candidates.minBy(_._2)._2
      })
    }
  }
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

case class SingleUnitBehaviourMeta(priority: SecondPriority, refuseCommandsForTicks: Int,
                                   forceRepeats: Boolean)

abstract class SingleUnitBehaviour[+T <: WrapsUnit](val unit: T, meta: SingleUnitBehaviourMeta)
  extends JobOrSubJob[T] {

  private var skipFor = 0

  override def universe = unit.universe

  def onStealUnit(): Unit = {}

  def orderForTick(what: Objective) = {
    if (skipFor > 0) {
      skipFor -= 1
      Nil
    } else {
      val interrupt = higherPriorityOrder
      if (interrupt.isEmpty) {
        toOrder(what)
      } else {
        interrupt
      }
    }
  }

  def preconditionOk = true

  def describeShort: String

  def priority = meta.priority

  def blocksForTicks = meta.refuseCommandsForTicks

  def forceRepeats = meta.forceRepeats

  def canInterrupt = true

  def skipFor(i: Int): Unit = {
    skipFor = i
  }

  protected def toOrder(what: Objective): Seq[UnitOrder]
}

class BusyDoingSomething[T <: WrapsUnit](employer: Employer[T],
                                         behaviour: Seq[SingleUnitBehaviour[T]],
                                         private var objective: Objective)
  extends UnitWithJob(employer, behaviour.head.unit, Priority.DefaultBehaviour) with
          Interruptable[T] {

  assert(behaviour.map(_.unit).distinct.size == 1, s"Wrong grouping: $behaviour")

  private var lastTickOrderIssuedBy = Option.empty[SingleUnitBehaviour[T]]
  private var lastOrderIssuedBy     = Option.empty[SingleUnitBehaviour[T]]

  override def onStealUnit() = {
    super.onStealUnit()
    behaviour.foreach(_.onStealUnit())
  }

  override def interruptableNow = super.interruptableNow &&
                                  lastTickOrderIssuedBy.exists(_.canInterrupt)

  def newObjective_!(objective: Objective): Unit = {
    this.objective = objective
  }

  override def shortDebugString = {
    val realOrder = lastTickOrderIssuedBy.map(_.describeShort).getOrElse("???")
    val lastOrder = lastOrderIssuedBy.map(_.describeShort).getOrElse("???")
    s"[BG] $realOrder ($lastOrder)"
  }

  override def renderDebug(renderer: Renderer) = {
    super.renderDebug(renderer)
    lastOrderIssuedBy.foreach(_.renderDebug(renderer))
  }

  // never ends
  override def isFinished = false

  override def ordersForTick = {
    val tmp = highestPriorityOrdersForTick
    // keep track of it for debugging purposes
    lastTickOrderIssuedBy = tmp._1
    lastOrderIssuedBy = lastTickOrderIssuedBy.orElse(lastOrderIssuedBy)

    tmp._2
  }

  private def highestPriorityOrdersForTick = {
    val options = active.map { rule =>
      rule -> rule.orderForTick(objective)
              .map(_.lockingFor_!(rule.blocksForTicks).forceRepeat_!(rule.forceRepeats))
    }.filter(_._2.nonEmpty)
    if (options.isEmpty) {
      None -> Nil
    } else {
      val (a, b) = options.maxBy(_._1.priority)
      Some(a) -> b
    }
  }

  private def active = behaviour.filter(_.preconditionOk)

  override protected def omitRepeatedOrders = true

}

class BusyDoingNothing[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.None) with IssueOrderNTimes[T] with
          Interruptable[T] {
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
    implicit val ordOnVectorWithDoubles: Ordering[Vector[Double]] = {
      val cmp = new Comparator[Vector[Double]] {
        override def compare(a: Vector[Double], b: Vector[Double]): Int = {
          var i = 0
          while (i < a.size) {
            val left = a(i)
            val right = b(i)
            if (left < right) return -1
            if (left > right) return 1
            i += 1
          }
          0
        }
      }
      Ordering.comparatorToOrdering(cmp)
    }
    Ordering.by(_.data)
  }

  def apply(singleValue: Double): PriorityChain = PriorityChain(Vector(singleValue))

  def apply(multiValues: Double*): PriorityChain = PriorityChain(multiValues.toVector)
}

object UnitRequest {

  private var counter = 0

  def nextId() = {
    counter += 1
    counter
  }

  object CherryPickers {
    def cherryPickWorkerByDistance[W <: WorkerUnit](target: MapTilePosition)
                                                   (distanceEvaluation: MapTilePosition => Double
                                                    = _
                                                      .distanceSquaredTo(
                                                        target)) =
      (job: UnitWithJob[W]) => {
        val u = job.universe
        val altUnit = job.unit
        val walkMap = u.mapLayers.rawWalkableMap

        val sameArea = walkMap.areInSameWalkableArea(altUnit.currentTile, target)
        val canSee = walkMap.connectedByLine(altUnit.currentTile, target)
        val distance = distanceEvaluation(altUnit.currentTile)
        val busyness = WorkerUnit.currentPriority(job)
        val weightedDistance = distance * busyness.sum
        PriorityChain(sameArea.ifElse(0, 1), canSee.ifElse(0, 1), weightedDistance)
      }
  }
}

trait UnitRequest[T <: WrapsUnit] {

  private val id                  = UnitRequest.nextId()
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
  def typeOfRequestedUnit: Class[_ <: T]
  def amount: Int
  def acceptableUntyped(unit: WrapsUnit) = includesByType(unit) &&
                                           acceptable(unit.asInstanceOf[T])
  def includesByType(unit: WrapsUnit): Boolean = typeOfRequestedUnit.isInstance(unit)
  def acceptable(unit: T) = filter.map(_.apply(unit)).getOrElse(true)
  def clearable = autoCleanAfterTick
  def dispose(): Unit = {
    trace(s"$debugString is being disposed of")
    onDisposeActions.foreach(_.onClear())
  }

  def debugString = s"Req[$id]$this"

  if (classOf[Building].isAssignableFrom(typeOfRequestedUnit)) {
    keepResourcesLocked_!()
  }

  def doOnDispose_![X](u: => X) = {
    onDisposeActions += (() => u)
  }

  def persistant_!(): Unit = {
    autoCleanAfterTick = false
  }

  def clearableInNextTick_!(): Unit = {
    trace(s"$debugString will be cleared next tick")
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

case class AnyUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[_ <: T], amount: Int)
  extends UnitRequest[T] {
  override def priority: Priority = Priority.Default

}

case class AnyFactoryRequest[T <: UnitFactory, U <: Mobile](typeOfRequestedUnit: Class[_ <: T],
                                                            amount: Int,
                                                            buildThis: Class[_ <: U])
  extends UnitRequest[T] {
  override def acceptable(unit: T): Boolean = {
    super.acceptable(unit) && unit.canBuild(buildThis)
  }

  override def priority: Priority = Priority.Default
}

trait OnClearAction {
  def onClear(): Unit
}

case class BuildUnitRequest[T <: WrapsUnit](universe: Universe, typeOfRequestedUnit: Class[_ <: T],
                                            amount: Int,
                                            funding: ResourceApproval,
                                            override val priority: Priority,
                                            customBuildingPosition: AlternativeBuildingSpot,
                                            belongsTo: Option[ResourceArea] = None)
  extends UnitRequest[T] with HasFunding with HasUniverse {

  self =>

  if (funding.isSuccess) {
    universe.resources.informUsage(funding, this)
  }

  lazy val isAddon = classOf[Addon].isAssignableFrom(typeOfRequestedUnit)

  lazy val isUpgrader = classOf[Upgrader].isAssignableFrom(typeOfRequestedUnit)

  lazy val isBuilding = classOf[Building].isAssignableFrom(typeOfRequestedUnit)

  lazy val isMobile = classOf[Mobile].isAssignableFrom(typeOfRequestedUnit)

  if (customBuildingPosition.shouldUse) {
    assert(typeOfRequestedUnit.toUnitType.isBuilding)
  }

  override def proofForFunding = funding

  override def acceptable(unit: T): Boolean = false // refuse all that exist

  def customPosition = customBuildingPosition

  override def dispose(): Unit = {
    super.dispose()
    // if this is a unit build request, dispose resources if the job itself has not already done so
    if (unlocksResourcesOnDispose) {
      assert(stillLocksResources)
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

case class UnitJobRequest[T <: WrapsUnit : Manifest](request: UnitRequest[T],
                                                     employer: Employer[T],
                                                     priority: Priority,
                                                     makeSureDependenciesCleared: Set[Class[_ <:
                                                       WrapsUnit]] =
                                                     Set.empty) {

  def withRequest(f: UnitRequest[T] => UnitRequest[T]) = {
    copy(request = f(request))
  }

  assert(requestedUnitType.isAssignableFrom(moreSpecificType),
    s"$moreSpecificType > $requestedUnitType")

  def canInterrupt(uwj: UnitWithJob[_ <: WrapsUnit]) = {
    assert(priority > uwj.priority, "Oops :(")
    // perfect solution: match every type against every type and use heavy logic to determine the
    // result
    // reality: lazily add cases
    uwj match {
      case i: Interruptable[_] if i.interruptableNow => true
      case _ => false
    }
  }

  def allRequiredTypes = request.typeOfRequestedUnit.toSet ++ makeSureDependenciesCleared

  def priorityRule: Option[RateCandidate[T]] = {
    val picker = request.ratingFuntion
    picker.map { nat =>
      new RateCandidate[T] {
        def giveRating(forThatOne: UnitWithJob[T]): PriorityChain = nat(forThatOne)
      }
    }
  }

  def withOnlyAccepting(these: T => Boolean) = {
    copy(request = request.withFilter_!(these))
  }

  def clearable = request.clearable

  def wantsUnit(existingUnit: WrapsUnit) = moreSpecificType.isInstance(existingUnit) &&
                                           request.acceptableUntyped(existingUnit)

  def moreSpecificType = request.typeOfRequestedUnit

  def requestedUnitType = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]

  def onClear(): Unit = request.dispose()
}

object UnitJobRequest {
  def upgraderFor(upgrade: Upgrade, employer: Employer[Upgrader],
                  priority: Priority = Priority.Upgrades) = {
    val actualClass = employer.race.techTree.upgraderFor(upgrade).asInstanceOf[Class[Upgrader]]
    val req = AnyUnitRequest(actualClass, 1)
    UnitJobRequest(req, employer, priority).withOnlyAccepting(!_.isDoingResearch)
  }

  def builderOf[T <: Mobile, F <: UnitFactory : Manifest](wantedType: Class[_ <: T],
                                                          employer: Employer[F],
                                                          priority: Priority = Priority
                                                                               .Default):
  UnitJobRequest[F] = {

    val actualClass = employer.universe.myRace.specialize(manifest[F].runtimeClass
                                                          .asInstanceOf[Class[F]])
    val req = AnyFactoryRequest[F, T](actualClass, 1, wantedType)

    UnitJobRequest(req, employer, priority)
  }

  def constructor[T <: WorkerUnit : Manifest](employer: Employer[T],
                                              priority: Priority = Priority
                                                                   .ConstructBuilding):
  UnitJobRequest[T] = {

    val actualClass = employer.universe.myRace.specialize(manifest[T].runtimeClass)
                      .asInstanceOf[Class[T]]
    val req = AnyUnitRequest(actualClass, 1)
              .withCherryPicker_!(WorkerUnit.currentPriority)

    UnitJobRequest(req, employer, priority)
  }

  def addonConstructor[T <: CanBuildAddons : Manifest](employer: Employer[T],
                                                       what: Class[_ <: Addon],
                                                       priority: Priority = Priority
                                                                            .ConstructBuilding) = {

    val actualClass = employer.universe.myRace.specialize(what)
    val mainType = employer.race.techTree.mainBuildingOf(what).asInstanceOf[Class[T]]
    val req = AnyUnitRequest(mainType, 1)
              .withFilter_!(e => !e.isBeingCreated && !e.hasAddonAttached && !e.isBuildingAddon)

    UnitJobRequest(req, employer, priority, Set(what))
  }

  def idleOfType[T <: WrapsUnit : Manifest](employer: Employer[T], ofType: Class[_ <: T],
                                            amount: Int = 1,
                                            priority: Priority = Priority.Default) = {
    val realType = employer.universe.myRace.specialize(ofType)
    val req: AnyUnitRequest[T] = AnyUnitRequest(realType, amount)

    UnitJobRequest(req, employer, priority)
  }

  def newOfType[T <: WrapsUnit : Manifest](universe: Universe, employer: Employer[T],
                                           ofType: Class[_ <: T],
                                           funding: ResourceApprovalSuccess, amount: Int = 1,
                                           priority: Priority = Priority.Default,
                                           customBuildingPosition: AlternativeBuildingSpot =
                                           AlternativeBuildingSpot
                                           .useDefault,
                                           belongsTo: Option[ResourceArea] = None) = {
    val actualType = universe.myRace.specialize(ofType)
    val req: BuildUnitRequest[T] = {
      BuildUnitRequest(universe, actualType, amount, funding, priority,
        customBuildingPosition, belongsTo)
    }
    // this one needs to survive across ticks
    req.persistant_!()
    UnitJobRequest[T](req, employer, priority)
  }
}

class SendOrdersToStarcraft(universe: Universe) extends AIModule[Controllable](universe) {
  override def ordersForTick: Traversable[UnitOrder] = {
    unitManager.allJobsByUnitType[Controllable].filterNot(_.failedOrObsolete).flatMap { job =>
      job.ordersForThisTick
    }
  }
}

class JobReAssignments(universe: Universe) extends OrderlessAIModule[Controllable](universe) {
  override def onTick_!(): Unit = {
    unitManager.nextJobReorganisationRequest
    .filter(_.stillWantsOptimization)
    .foreach { optimizeMe =>
      trace(s"Trying to find better unit for job $optimizeMe")
      if (optimizeMe.hasToSwitchLater) {
        // try again later
        trace(s"Not found, try again later")
        unitManager.tryFindBetterEmployeeFor(optimizeMe)
      } else {
        def doTyped[T <: WrapsUnit : Manifest](old: CanAcceptUnitSwitch[T]) = {
          val uc = new UnitCollector(old.asRequest, universe).collect_!(Set(old))
          uc match {
            case Some(replacement) if replacement.hasOneMember &&
                                      replacement.onlyMember == optimizeMe.unit =>
              trace(s"Same unit chosen as best worker")
            case Some(replacement) if replacement.hasOneMember &&
                                      replacement.onlyMember != optimizeMe.unit =>
              info(s"Replacement found: ${replacement.onlyMember}")
              //someone else might have already done this somewhere else
              val nobody = unitManager.Nobody
              if (unitManager.employerOf(optimizeMe.unit).contains(nobody)) {
                warn(s"Unit ${optimizeMe.unit} was already asigned to $nobody")
              } else {
                unitManager.assignJob_!(new BusyDoingNothing(optimizeMe.unit, nobody))
              }
              unitManager.assignJob_!(old.copyOfJobForNewUnit(replacement.onlyMember))
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

