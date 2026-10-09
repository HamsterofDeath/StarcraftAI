package pony
package brain

import pony.Orders.Stop

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
    requestedToBuild.exists(e => c >= e.typeOfRequestedUnit) ||
    plannedToTrain.exists(e => c >= e.typeOfRequestedUnit)
  }

  def countExistingAndPlanned(c: Class[_ <: WrapsUnit]) = {
    ownUnits.allByClass(c).size +
    requestedToBuild.count(e => c >= e.typeOfRequestedUnit) +
    plannedToTrain.count(e => c >= e.typeOfRequestedUnit)
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
    val typeOfFactory = implicitly[Manifest[T]].runtimeClass.asInstanceOf[Class[_ <: T]]
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
    val typeOfFactory = implicitly[Manifest[T]].runtimeClass.asInstanceOf[Class[_ <: T]]
    unfulfilledByTargetType(typeOfFactory)
  }

  def constructionsInProgress[T <: Building : Manifest]: Seq[ConstructBuilding[WorkerUnit, T]] = {
    constructionsInProgress(implicitly[Manifest[T]].runtimeClass.asInstanceOf[Class[_ <: T]])
  }

  def constructionsInProgress[T <: Building](typeOfBuilding: Class[_ <: T]):
  Seq[ConstructBuilding[WorkerUnit, T]] = {
    val byJob = allJobsByType[ConstructBuilding[WorkerUnit, Building]].collect {
      case cr: ConstructBuilding[WorkerUnit, Building]
        if typeOfBuilding >= cr.typeOfBuilding =>
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
    val wanted = implicitly[Manifest[T]].runtimeClass
    assignments.valuesIterator.filter { job =>
      wanted >= job.getClass
    }.map {_.asInstanceOf[T]}.toVector
  }

  def allJobsByUnitType[T <: WrapsUnit : Manifest] = selectJobs[T, UnitWithJob[T]](_ => true)

  def selectJobs[U <: WrapsUnit : Manifest, T <: UnitWithJob[U]](f: T => Boolean) = {
    val wanted = implicitly[Manifest[U]].runtimeClass
    assignments.valuesIterator.filter { job =>
      wanted.isInstance(job.unit) && f(job.asInstanceOf[T])
    }.map {_.asInstanceOf[T]}.toVector
  }

  def failedToProvideByType[T <: WrapsUnit : Manifest] = {
    val c = implicitly[Manifest[T]].runtimeClass
    failedToProvideFlat.collect {
      case req: UnitRequest[_] if c >= req.typeOfRequestedUnit =>
        req.asInstanceOf[UnitRequest[T]]
    }
  }

  def failedToProvideFlat = failedToProvide.map(_.request)

  def failedToProvide = unfulfilledRequestsLastTick

  def jobOptOf[T <: WrapsUnit](unit: T) = assignments.get(unit).asInstanceOf[Option[UnitWithJob[T]]]

  def jobOf[T <: WrapsUnit](unit: T) = assignments(unit).asInstanceOf[UnitWithJob[T]]

  def tick(): Unit = {
    // do not pile these up, clear per tick - this is why unitmanagers tick must come last.
    val (clearable, keep) = unfulfilledRequestsLastTick.partition(_.clearable)
    clearable.foreach(_.onClear())
    unfulfilledRequestsLastTick = unfulfilledRequestsThisTick.toVector ++ keep
    unfulfilledRequestsThisTick.clear()

    //clean/update
    ownUnits.allKnownUnits.foreach(_.onTick_!())
    assignments.valuesIterator.foreach(_.onTick_!())
    enemies.allKnownUnits.foreach(_.onTick_!())
    val removeUs = {
      val done = assignments.iterator.collect { case (_, job) if job.isFinished => job }.toVector
      val failed = assignments.iterator.collect { case (_, job) if job.failedOrObsolete => job }
                   .toVector

      trace(s"${failed.size}/${done.size} jobs failed/finished, putting units on the market again",
        failed.nonEmpty || done.nonEmpty)

      failed.foreach { failure =>
        info(s"FAIL! $failure")
      }
      trace(s"Failed: ${failed.mkString("\n")}")
      trace(s"Finished: ${done.mkString(", ")}")
      val ret = done ++ failed
      assert(ret.size == ret.distinct.size, s"A job is failed and finished at the same time")
      ret
    }
    trace(s"Cleaning up ${removeUs.size} finished/failed jobs", removeUs.nonEmpty)

    removeUs.foreach { job =>
      if (universe.currentTick < 6000 && job.unit.isInstanceOf[WorkerUnit])
        NativeMatchEvidence.trace("job-removed",
          s"${job.getClass.getSimpleName} #${job.unit.nativeUnitId} ${job.failureDebug}")
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

    val myOwn = universe
                .ownUnits
                .inFaction
                .filterNot(assignments.contains)
                .flatMap(e => initialJobOf(e).toList)
                .toSeq
    info(s"Found ${myOwn.size} new units of player", myOwn.nonEmpty)

    myOwn.foreach(assignJob_!)
    assignments ++= myOwn.map(e => e.unit -> e)
    if (universe.currentTick < 3000 && universe.currentTick % 24 == 0)
      NativeMatchEvidence.trace("um-tick",
        s"known=${universe.ownUnits.allKnownUnits.size} new=${myOwn.size} assigned=${assignments.size} nobody=${assignments.count(_._2.employer == Nobody)}")

    val registerUs = universe
                     .ownUnits
                     .allKnownUnits
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
    val (m, i, p, j) = findMissingRequirements[T](collection.immutable.Set(c))
    m.isEmpty && i.isEmpty && p.isEmpty && j.isEmpty
  }

  def requirementsQueuedToBuild[T <: WrapsUnit](c: Class[_ <: T]) = {
    val (_, i, p, j) = findMissingRequirements[T](collection.immutable.Set(c))
    (i.nonEmpty || j.nonEmpty) && p.isEmpty
  }

  private def findMissingRequirements[T <: WrapsUnit](c: Set[Class[_ <: T]]) = {
    val mustHave = c.flatMap(race.techTree.requiredFor)
    val m = mutable.Set.empty[Class[_ <: Building]]
    val i = mutable.Set.empty[Class[_ <: Building]]
    val p = mutable.Set.empty[Class[_ <: Building]]
    val j = mutable.Set.empty[Class[_ <: Building]]
    lazy val jobs = unitManager.allJobsByType[ConstructBuilding[_, _]]

    val missing = {
      mustHave.foreach { dependency =>
        val complete = ownUnits.existsComplete(dependency)
        if (!complete) {
          val incomplete = ownUnits.existsIncomplete(dependency)
          if (incomplete) {
            i += dependency
          } else {
            val requested = unitManager.requestedToBuild(dependency)
            if (requested) {
              p += dependency
            } else {
              val coveredByJob = jobs.exists(_.typeOfBuilding >= dependency)
              if (coveredByJob) {
                j += dependency
              } else {
                m += dependency
              }
            }
          }
        }
      }
    }
    (m.toSet, i.toSet, p.toSet, j.toSet)
  }

  def requestedToBuild(c: Class[_ <: Building]): Boolean = {
    requestedToBuild.exists { e =>
      c >= e.typeOfRequestedUnit
    }
  }

  def requestedToBuild = {
    allUnfulfilled.map(_.request).collect {
      case b: BuildUnitRequest[_] if b.isBuilding => b
    }
  }

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
    if (req.request.amount == 0) {
      new FailedPreHiringResult[T]
    } else {
      trace(s"${req.employer} requested ${req.request.toString}")
      val (missing, incomplete, planned, jobbed) = findMissingRequirements(req.allRequiredTypes)
      val result = {
        if (missing.isEmpty && incomplete.isEmpty && planned.isEmpty && jobbed.isEmpty) {
          val hr = collectCandidates(req)
          hr match {
            case None =>
              if (universe.currentTick < 3000)
                NativeMatchEvidence.trace("hire-none",
                  s"employer=${req.employer} type=${req.requestedUnitType.getSimpleName} assigned=${assignments.size} nobody=${assignments.count(_._2.employer == Nobody)} idle=${assignments.count(_._2.isIdle)} workers=" +
                    assignments.collect { case (u, j) if u.isInstanceOf[WorkerUnit] => s"#${u.nativeUnitId}:${j.getClass.getSimpleName}:${j.priority}:${j.isIdle}" }.mkString(","))
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
          new MissingRequirementResult[T](missing, incomplete, planned, jobbed)
        }
      }

      trace(s"Result of request: $result")
      result
    }
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

  case object Nobody extends Employer[WrapsUnit](universe)

  case object Trainer extends Employer[WrapsUnit](universe)

  case object Constructor extends Employer[WrapsUnit](universe)

}
