package pony
package brain

import pony.Orders.Stop

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class UnitManager(override val universe: Universe) extends HasUniverse {
  private val reorganizeJobQueue          = ListBuffer.empty[CanAcceptUnitSwitch[_ <: WrapsUnit]]
  private val unfulfilledRequestsThisTick = ArrayBuffer.empty[UnitJobRequests[_ <: WrapsUnit]]
  private val assignments                 = mutable.HashMap.empty[WrapsUnit, UnitWithJob[_ <: WrapsUnit]]
  private val byEmployer                  = new
      mutable.HashMap[Employer[_ <: WrapsUnit], mutable.Set[UnitWithJob[_ <: WrapsUnit]]]
      with mutable.MultiMap[Employer[_ <: WrapsUnit], UnitWithJob[_ <: WrapsUnit]]
  private var unfulfilledRequestsLastTick = unfulfilledRequestsThisTick.toVector
  def nextJobReorganisationRequest = {
    val ret = reorganizeJobQueue.lastOption
    if (reorganizeJobQueue.nonEmpty) {
      reorganizeJobQueue.remove(0)
    }
    trace(s"${reorganizeJobQueue.size} jobs left to optimize")
    ret
  }
  def tryFindBetterEmployeeFor[T <: WrapsUnit](anyJob: CanAcceptUnitSwitch[T]): Unit = {
    reorganizeJobQueue += anyJob
  }
  def plannedToBuild = unfulfilledRequestsLastTick.flatMap(_.requests).collect { case b: BuildUnitRequest[_] => b }

  def plannedToBuildByType(typeOfFactory: Class[_ <: Building]): Int = {
    val byUnfulfilledRequest = unfulfilledRequestsLastTick.flatMap(_.requests).iterator.collect {
      case b: BuildUnitRequest[_] if b.typeOfRequestedUnit == typeOfFactory => 1
    }.size
    byUnfulfilledRequest
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
    val byUnfulfilledRequest = unfulfilledRequestsLastTick.flatMap(_.requests).collect {
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
      wanted.isAssignableFrom(job.unit.getClass) && f(job.asInstanceOf[T])
    }.map {_.asInstanceOf[T]}.toVector
  }
  def race = bases.mainBase.mainBuilding.race
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

    //clean
    assignments.foreach(_._2.onTick())
    val removeUs = {
      val done = assignments.filter { case (_, job) => job.isFinished }.values
      val failed = assignments.filter { case (_, job) => job.hasFailed }.values
      debug(s"${failed.size} jobs failed, putting units on the market again", failed.nonEmpty)
      trace(s"Failed: ${failed.mkString(", ")}")
      trace(s"Finished: ${done.mkString(", ")}")
      val ret = (done ++ failed).toVector
      assert(ret.size == ret.distinct.size, s"A job is failed and finished at the same time")
      ret
    }
    debug(s"Cleaning up ${removeUs.size} finished/failed jobs", removeUs.nonEmpty)

    removeUs.foreach { job =>
      job.unit match {
        case m: Mobile =>
          // stop whatever you were doing so the next employer doesn't hire a rebel
          world.orderQueue.queue_!(new Stop(m))
        case _ =>
      }

      val newJob = new BusyDoingNothing(job.unit, Nobody)
      assignJob_!(newJob)
      job.onFinish()
    }

    def jobOf[T <: WrapsUnit](unit: T) = {
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

    val myOwn = universe.world.units.mine.filterNot(assignments.contains).map(jobOf).toSeq
    info(s"Found ${myOwn.size} new units of player", myOwn.nonEmpty)

    myOwn.foreach(assignJob_!)
    assignments ++= myOwn.map(e => e.unit -> e)

    val registerUs = universe.world.units.all.filterNot(assignments.contains).map(jobOf).toSeq
    info(s"Found ${registerUs.size} new units (not of player)", registerUs.nonEmpty)
    assignments ++= registerUs.map(e => e.unit -> e)

  }

  def assignJob_![T <: WrapsUnit](newJob: UnitWithJob[T]): Unit = {
    val employer = newJob.employer
    trace(s"New job assignment: $newJob, employed by $employer")
    val newUnit = newJob.unit
    employer.hire_!(newUnit)
    assignments.get(newUnit).foreach { oldJob =>
      assert(oldJob.unit eq newUnit, s"${oldJob.unit} is not ${newUnit}")
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

  def request[T <: WrapsUnit : Manifest](req: UnitJobRequests[T], buildIfNoneAvailable: Boolean = true) = {
    trace(s"${req.employer} requested ${req.requests.mkString(" and ")}")
    def hireResult: Option[UnitCollector[T]] = {
      new UnitCollector(req, universe).collect_!()
    }

    val hr = hireResult
    val result = hr match {
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
  def success: Boolean
  def canHire: Map[UnitRequest[T], Set[T]]
  def units = canHire.flatMap(_._2)

  override def toString = s"HiringResult($success, $canHire)"
}

class FailedPreHiringResult[T <: WrapsUnit] extends PreHiringResult[T] {
  def success = false
  def canHire = Map.empty
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

  private val hired                      = new
      mutable.HashMap[UnitRequest[T], mutable.Set[T]] with mutable.MultiMap[UnitRequest[T], T]
  private val remainingAmountsPerRequest = mutable.HashMap.empty ++
                                           req.requests.map { e => e -> e.amount }.toMap
  def onlyMember = {
    assert(hasOneMember)
    hired.values.head.head
  }
  def hasOneMember = hired.valuesIterator.map(_.size).sum == 1
  def collect_!(): Option[UnitCollector[T]] = {
    val um = unitManager
    val available = {
      val potential = (um.allOfEmployer(um.Nobody) ++ um.allNotOfEmployer(um.Nobody))
                      .filter(_.priority < req.priority)
                      .filter(requests)
                      .map(typed)
      priorityRule.fold(potential.toVector) { rule =>
        potential.toVector.sortBy(rule.giveRating)
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
      case (req, remaining) if remaining > 0 && req.includesByType(unit) => true
      case _ => false
    }.foreach { case (req, missing) =>
      remainingAmountsPerRequest.put(req, missing - 1)
      // we know the type because we asked req before
      hired.addBinding(req, unit.asInstanceOf[T])
    }
  }
  def complete = remainingAmountsPerRequest.valuesIterator.sum == 0
  def requests(unit: UnitWithJob[_ <: WrapsUnit]) = {
    req.wantsUnit(unit.unit)
  }
  def hasPriorityRule = priorityRule.isDefined
  def priorityRule = req.priorityRule
  def missingAsRequest: UnitJobRequests[T] = {
    val typesAndAmounts = remainingAmountsPerRequest.filter(_._2 > 0).map { case (req, amount) =>
      BuildUnitRequest[T](universe, req.typeOfRequestedUnit, amount, ResourceApprovalFail, Priority.Default)
    }
    UnitJobRequests[T](typesAndAmounts.toSeq, req.employer, req.priority)
  }
  override def toString: String = s"Collected: ${teamAsMap}"
  def teamAsMap = hired.toSeq.map { case (k, v) => k -> v.toSet }.toMap
}

class Employer[T <: WrapsUnit : Manifest](override val universe: Universe) extends HasUniverse {

  private var employees = ArrayBuffer.empty[T]

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
    info(s"$unit got hired by someone else")
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

abstract class UnitWithJob[T <: WrapsUnit](val employer: Employer[T], val unit: T, val priority: Priority)
  extends HasUniverse {
  override val universe           = employer.universe
  private  val creationTick       = currentTick
  private  val listeners          = ArrayBuffer.empty[JobFinishedListener[T]]
  private  var noCommandsForTicks = 0
  def onTick(): Unit = {
    unit.onTick(universe)
  }
  def onStealUnit() = {}
  def shortDebugString: String
  def age = currentTick - creationTick
  def isIdle: Boolean = false
  def ordersForThisTick = {
    if (noCommandsForTicks > 0) {
      noCommandsForTicks -= 1
      Nil
    } else {
      ordersForTick
    }
  }
  def noCommandsForTicks_!(n: Int): Unit = {
    noCommandsForTicks = n
  }
  override def toString: String = s"${getClass.className} of $unit of $employer"
  def isFinished: Boolean
  def onFinish(): Unit = {
    listeners.foreach(_.onFinish())
  }
  def listen_!(listener: JobFinishedListener[T]): Unit = listeners += listener
  def hasFailed: Boolean = false
  protected def ordersForTick: Seq[UnitOrder]
}

trait IssueOrderNTimes[T <: WrapsUnit] extends UnitWithJob[T] {
  private var issued = 0
  def once: Seq[UnitOrder]
  override def ordersForTick: Seq[UnitOrder] = {
    if (issued == times)
      Nil
    else {
      issued += times
      once
    }
  }
  def times = 1
}

trait HasFunding {
  private var explicitlyUnlocked = false
  private var autoUnlocked       = false
  def proofForFunding: ResourceApproval
  def resources: ResourceManager
  def unlockManually_!(): Unit = {
    trace(s"Manually unlocking $proofForFunding of $this")
    unlock_!()
    explicitlyUnlocked = true
  }
  def unlock_!(): Unit = {
    if (!explicitlyUnlocked) {
      assert(!autoUnlocked)
      proofForFunding match {
        case suc: ResourceApprovalSuccess =>
          autoUnlocked = true
          resources.unlock_!(suc)
        case _ =>
      }
    } else {
      trace(s"Not unlocking $proofForFunding of $this automatically, someone already did that")
    }
  }
}

trait JobHasFunding[T <: WrapsUnit] extends UnitWithJob[T] with HasUniverse with HasFunding {

  listen_!(() => {
    unlock_!()
  })
}

trait JobFinishedListener[T <: WrapsUnit] {
  def onFinish(): Unit
}

trait CreatesUnit[T <: WrapsUnit] extends UnitWithJob[T]

class TrainUnit[F <: UnitFactory, T <: Mobile](factory: F, trainType: Class[_ <: T], employer: Employer[F],
                                               funding: ResourceApprovalSuccess)
  extends UnitWithJob[F](employer, factory, Priority.Default) with JobHasFunding[F] with IssueOrderNTimes[F] with
          CreatesUnit[F] {

  override def proofForFunding = funding

  override def once: Seq[UnitOrder] = {
    Orders.Train(unit, trainType).toSeq
  }

  override def isFinished = {
    val idle = !factory.isProducing
    val delay = age > 10
    val isLazy = !factory.nativeUnit.isTraining
    val ret = idle && delay && isLazy
    ret
  }

  override def shortDebugString: String = s"Train ${trainType.className}"
}

class ConstructBuilding[W <: WorkerUnit : Manifest, B <: Building](worker: W, buildingType: Class[_ <: B],
                                                                   employer: Employer[W],
                                                                   val where: MapTilePosition,
                                                                   funding: ResourceApprovalSuccess)
  extends UnitWithJob[W](employer, worker, Priority.ConstructBuilding) with JobHasFunding[W] with CreatesUnit[W] with
          IssueOrderNTimes[W] with CanAcceptUnitSwitch[W] {

  val area = {
    val unitType = buildingType.toUnitType
    val size = Size.shared(unitType.tileWidth(), unitType.tileHeight())
    Area(where, size)
  }
  private var startedMovingToSite      = false
  private var startedActualConstuction = false
  private var finishedConstruction     = false
  private var resourcesUnlocked        = false
  override def onTick(): Unit = {
    super.onTick()
    if (startedActualConstuction && !resourcesUnlocked) {
      unlockManually_!()
      resourcesUnlocked = true
    }
  }

  override def once: Seq[UnitOrder] = Orders.Construct(worker, buildingType, where).toSeq

  override def times = 10

  override def shortDebugString: String = s"Build ${buildingType.className}"

  def typeOfBuilding = buildingType

  override def proofForFunding = funding

  override def canSwitchNow = !startedActualConstuction

  override def couldSwitchInTheFuture = !startedActualConstuction

  override def hasFailed: Boolean = {
    val fail = !worker.isInConstructionProcess && age > 50 && !isFinished
    fail
  }

  override def isFinished = {
    if (!startedMovingToSite) {
      startedMovingToSite = worker.isInConstructionProcess
      trace(s"Worker $worker started to move to construction site")
    } else if (!startedActualConstuction) {
      startedActualConstuction = worker.isConstructingBuilding
      trace(s"Worker $worker started to build $buildingType")
    } else if (!finishedConstruction) {
      finishedConstruction = !worker.isInConstructionProcess
      trace(s"Worker $worker finished to build $buildingType")
    }
    age > 50 && startedMovingToSite && finishedConstruction
  }

  override def asRequest: UnitJobRequests[W] = {
    val request = AnyUnitRequest[W](worker.getClass, 1)
    val anyUnitRequest = request.withCherryPicker_! { hijackFromThis =>
      val altUnit = hijackFromThis.unit
      val sameArea = mapLayers.rawWalkableMap.areaWhichContains(altUnit.currentTile) ==
                     mapLayers.rawWalkableMap.areaWhichContains(where)

      val canSee = mapLayers.rawWalkableMap.connectedByLine(altUnit.currentTile, where)

      val distance = area.distanceTo(altUnit.currentTile)

      PriorityChain(sameArea.ifElse(1, 0), canSee.ifElse(1, 0), -distance)
    }
    UnitJobRequests(anyUnitRequest.toSeq, employer, priority)
  }

  override def newFor(replacement: W) = new ConstructBuilding(replacement, buildingType, employer, where, funding)
}

class BusyDoingNothing[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.None) {
  override def isIdle = true
  override def ordersForTick: Seq[UnitOrder] = Nil
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

case class PriorityChain(data: Vector[Double])
object PriorityChain {

  implicit         val ordOnPriorities       : Ordering[PriorityChain]  = Ordering.by(_.data)
  private implicit val ordOnVectorWithDoubles: Ordering[Vector[Double]] = Ordering.fromLessThan { (a, b) =>
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
  def apply(singleValue: Double): PriorityChain = PriorityChain(Vector(singleValue))
  def apply(multiValues: Double*): PriorityChain = PriorityChain(multiValues.toVector)
}

trait UnitRequest[T <: WrapsUnit] {

  private val onClearActions      = ArrayBuffer.empty[OnClearAction]
  private var picker              = Option.empty[UnitWithJob[T] => PriorityChain]
  private var autoCleanAfterTick  = true
  private var keepResourcesLocked = false
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
  def acceptable(unit: T) = true
  def clearable = autoCleanAfterTick
  def onClear(): Unit = {
    onClearActions.foreach(_.onClear())
  }
  def doOnClear_![X](u: => X) = {
    onClearActions += (() => u)
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
  def keepResourcesLocked_!(): Unit = {
    keepResourcesLocked = true
  }

  def unlocksResourcesOnClean = !keepResourcesLocked

  override def toString = s"UnitRequest($typeOfRequestedUnit, $amount)"
}

case class AnyUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[_ <: T], amount: Int) extends UnitRequest[T] {
  override def priority: Priority = Priority.Default

}

case class AnyFactoryRequest[T <: UnitFactory, U <: Mobile](typeOfRequestedUnit: Class[_ <: T], amount: Int,
                                                            buildThis: Class[_ <: U]) extends UnitRequest[T] {
  override def acceptable(unit: T): Boolean = {
    unit.canBuild(buildThis)
  }
  override def priority: Priority = Priority.Default
}

trait OnClearAction {
  def onClear(): Unit
}

case class BuildUnitRequest[T <: WrapsUnit](universe: Universe, typeOfRequestedUnit: Class[_ <: T], amount: Int,
                                            funding: ResourceApproval, override val priority: Priority)
  extends UnitRequest[T] with HasFunding with HasUniverse {

  override def proofForFunding = funding

  override def acceptable(unit: T): Boolean = false // refuse all that exist

  override def onClear(): Unit = {
    super.onClear()
    // for buildings, the resources need to be locked until the building process has begun
    if (unlocksResourcesOnClean) {
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
                                                      priority: Priority) {
  assert(requests.forall(_.clearable) || requests.forall(!_.clearable))

  private val types = requests.map(_.typeOfRequestedUnit).toSet
  def priorityRule: Option[RateCandidate[T]] = {
    val picker = if (requests.size == 1) requests.head.ratingFuntion else None
    picker.map { nat => new RateCandidate[T] {
      def giveRating(forThatOne: UnitWithJob[T]): PriorityChain = nat(forThatOne)
    }
    }
  }
  def clearable = requests.forall(_.clearable)

  def wantsUnit(existingUnit: WrapsUnit) = types.exists(_.isAssignableFrom(existingUnit.getClass)) &&
                                           requests.exists(_.acceptableUntyped(existingUnit))

  def commonUnitType = manifest[T].runtimeClass.asInstanceOf[Class[_ <: T]]

  def onClear(): Unit = requests.foreach(_.onClear())
}

object UnitJobRequests {
  def builderOf[T <: Mobile, F <: UnitFactory : Manifest](wantedType: Class[_ <: T],
                                                          employer: Employer[F],
                                                          priority: Priority = Priority.Default): UnitJobRequests[F] = {

    val actualClass = manifest[F].runtimeClass.asInstanceOf[Class[F]]
    val req = AnyFactoryRequest[F, T](actualClass, 1, wantedType)

    UnitJobRequests(req.toSeq, employer, priority)

  }

  def constructor[T <: WorkerUnit : Manifest](employer: Employer[T],
                                              priority: Priority = Priority.ConstructBuilding): UnitJobRequests[T] = {

    val actualClass = manifest[T].runtimeClass.asInstanceOf[Class[T]]
    val req = AnyUnitRequest(actualClass, 1).withCherryPicker_! { w =>
      var valueOfExcuses = 0
      if (!w.isIdle) valueOfExcuses += 1
      if (w.unit.isCarryingMinerals) valueOfExcuses += 2
      if (w.unit.isInMiningProcess) valueOfExcuses += 1
      PriorityChain(valueOfExcuses)
    }

    UnitJobRequests(req.toSeq, employer, priority)
  }

  def idleOfType[T <: WrapsUnit : Manifest](employer: Employer[T], ofType: Class[_ <: T], amount: Int = 1,
                                            priority: Priority = Priority.Default) = {
    val req: AnyUnitRequest[T] = AnyUnitRequest(ofType, amount)

    UnitJobRequests[T](req.toSeq, employer, priority)
  }

  def newOfType[T <: WrapsUnit : Manifest](universe: Universe, employer: Employer[T], ofType: Class[_ <: T],
                                           funding: ResourceApprovalSuccess, amount: Int = 1,
                                           priority: Priority = Priority.Default) = {
    val req: BuildUnitRequest[T] = BuildUnitRequest(universe, ofType, amount, funding, priority)
    // this one needs to survive across ticks
    req.persistant_!()
    UnitJobRequests[T](req.toSeq, employer, priority)
  }
}

class OrderBridge(universe: Universe) extends AIModule[Controllable](universe) {
  override def ordersForTick: Traversable[UnitOrder] = {
    unitManager.allJobsByUnitType[Controllable].flatMap { job =>
      job.ordersForThisTick
    }
  }
}

class JobReAssignments(universe: Universe) extends OrderlessAIModule[Controllable](universe) {
  override def onTick(): Unit = {
    unitManager.nextJobReorganisationRequest.foreach { optimizeMe =>
      debug(s"Trying to find better unit for job $optimizeMe")
      if (optimizeMe.hasToSwitchLater) {
        // try again later
        debug(s"Not found, try again later")
        unitManager.tryFindBetterEmployeeFor(optimizeMe)
      } else {
        def doTyped[T <: WrapsUnit : Manifest](old: CanAcceptUnitSwitch[T]) = {
          val uc = new UnitCollector(old.asRequest, universe).collect_!()
          uc match {
            case Some(replacement) if replacement.hasOneMember && replacement.onlyMember == optimizeMe.unit =>
              debug(s"Same unit chosen as best worker")
            case Some(replacement) if replacement.hasOneMember && replacement.onlyMember != optimizeMe.unit =>
              info(s"Replacement found: ${replacement.onlyMember}")
              unitManager.assignJob_!(new BusyDoingNothing(optimizeMe.unit, unitManager.Nobody))
              unitManager.assignJob_!(old.newFor(replacement.onlyMember))
            case _ =>
              if (optimizeMe.couldSwitchInTheFuture) {
                // try again later
                debug(s"Not found, try again later")
                unitManager.tryFindBetterEmployeeFor(optimizeMe)
              }
          }
        }
        doTyped(optimizeMe)
      }
    }
  }
}

