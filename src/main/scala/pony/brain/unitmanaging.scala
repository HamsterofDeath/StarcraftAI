package pony
package brain

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class UnitManager(override val universe: Universe) extends HasUniverse {
  private val unfulfilledRequests = ArrayBuffer.empty[UnitJobRequests[_ <: WrapsUnit]]
  private val assignments = mutable.HashMap.empty[WrapsUnit, UnitWithJob[_ <: WrapsUnit]]
  private val byEmployer  = new
      mutable.HashMap[Employer[_ <: WrapsUnit], mutable.Set[UnitWithJob[_ <: WrapsUnit]]]
      with mutable.MultiMap[Employer[_ <: WrapsUnit], UnitWithJob[_ <: WrapsUnit]]
  def race = bases.mainBase.mainBuilding.race
  def selectJobs[T <: UnitWithJob[_] : Manifest](f: T => Boolean) = {
    val wanted = manifest[T].runtimeClass
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

  def failedToProvide = unfulfilledRequests.toSeq

  def jobOf[T <: WrapsUnit](unit: T) = assignments(unit).asInstanceOf[UnitWithJob[T]]
  def tick(): Unit = {
    // do not pile these up, clear per tick
    unfulfilledRequests.clear()

    //clean
    val removeUs = assignments.filter { case (_, job) => job.isFinished }.values.toList
    info(s"Cleaning up ${removeUs.size} finished jobs", removeUs.nonEmpty)

    removeUs.foreach { job =>
      val newJob = new BusyDoingNothing(job.unit, Nobody)
      assignJob(Nobody, newJob)
      job.onFinish()

    }

    def jobOf[T <: WrapsUnit](unit: T) = {
      if (unit.isBeingCreated) {
        new BusyBeingTrained(unit, Nobody)
      } else {
        new BusyDoingNothing(unit, Nobody)
      }
    }

    val myOwn = universe.world.units.mine.filterNot(assignments.contains).map(jobOf).toSeq
    info(s"Found ${myOwn.size} new units of player", myOwn.nonEmpty)
    myOwn.foreach(byEmployer.addBinding(Nobody, _))
    assignments ++= myOwn.map(e => e.unit -> e)

    val registerUs = universe.world.units.all.filterNot(assignments.contains).map(jobOf).toSeq
    info(s"Found ${registerUs.size} new units (not of player)", registerUs.nonEmpty)
    assignments ++= registerUs.map(e => e.unit -> e)

  }
  def assignJob[T <: WrapsUnit](employer: Employer[T], newJob: UnitWithJob[T]): Unit = {
    trace(s"New job assignment: $newJob, employed by $employer")
    assignments.get(newJob.unit).foreach { oldJob =>
      val oldAssignment = byEmployer.find(_._2(oldJob))
      oldAssignment.foreach { case (oldEmployer, _) =>
        trace(s"Old job assignment was: $oldJob, employed by $oldEmployer")
        byEmployer.removeBinding(oldEmployer, oldJob)
      }
    }

    assignments.put(newJob.unit, newJob)
    byEmployer.addBinding(employer, newJob)

  }
  def request[T <: WrapsUnit : Manifest](req: UnitJobRequests[T]) = {
    trace(s"${req.employer} requested ${req.requests.mkString(" and ")}")
    def hireResult: Option[UnitCollector[T]] = {
      val collector = new UnitCollector(req)
      val available = (allOfEmployer(Nobody) ++ allNotOfEmployer(Nobody)).filter(_.priority < req.priority)
      available.foreach { candidate =>
        if (collector.requests(candidate)) {
          collector.collect(candidate.unit)
        }

        if (collector.complete) {
          return Some(collector)
        }
      }
      if (collector.hasAny)
        Some(collector)
      else
        None
    }

    val hr = hireResult
    val result = hr match {
      case None =>
        unfulfilledRequests += req
        new FailedPreHiringResult[T]
      case Some(team) if !team.complete =>
        unfulfilledRequests += team.missingAsRequest
        if (team.hasOneMember)
          new PartialPreHiringResult(team.teamAsMap) with ExactlyOneSuccess[T]
        else
          new PartialPreHiringResult(team.teamAsMap)
      case Some(team) =>
        if (team.hasOneMember) {
          new SuccessfulPreHiringResult(team.teamAsMap) with ExactlyOneSuccess[T]
        } else {
          new SuccessfulPreHiringResult(team.teamAsMap)
        }
    }
    trace(s"Result of request: $result")
    result
  }

  private def allOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.getOrElse(employer, Set.empty)
  private def allNotOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.filter(_._1 != employer)
                                                                        .flatMap(_._2)
  def nobody = Nobody

  case object Nobody extends Employer[WrapsUnit](universe)
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

class UnitCollector[T <: WrapsUnit : Manifest](originalRequest: UnitJobRequests[T]) {
  private val hired                      = new
      mutable.HashMap[UnitRequest[T], mutable.Set[T]] with mutable.MultiMap[UnitRequest[T], T]
  private val remainingAmountsPerRequest = mutable.HashMap.empty ++
                                           originalRequest.requests.map { e => e -> e.amount }.toMap
  def hasOneMember = hired.valuesIterator.map(_.size).sum == 1
  def missingAsRequest: UnitJobRequests[T] = {
    val typesAndAmounts = remainingAmountsPerRequest.filter(_._2 > 0).map { case (req, amount) =>
      BuildUnitRequest[T](req.typeOfRequestedUnit, amount)
    }
    UnitJobRequests[T](typesAndAmounts.toSeq, originalRequest.employer, originalRequest.priority)
  }
  def hasAny = hired.nonEmpty
  def teamAsMap = hired.toSeq.map { case (k, v) => k -> v.toSet }.toMap

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
    originalRequest.wantsUnit(unit.unit)
  }
}

class Employer[T <: WrapsUnit : Manifest](override val universe: Universe) extends HasUniverse {

  private var employees = ArrayBuffer.empty[T]

  def teamSize = employees.size

  def idleUnits = employees.filter(unitManager.jobOf(_).isIdle)

  def hire(result: PreHiringResult[T]): Unit = {
    result.units.foreach(hire)
  }

  def hire(unit: T): Unit = {
    employees += unit
  }

  def fire(unit: T): Unit = {
    employees -= unit
  }

  def assignJob(job: UnitWithJob[T]): Unit = {
    assert(employees.contains(job.unit), s"Please hire ${job.unit} before giving it a job")
    unitManager.assignJob(this, job)
  }

  def current = employees.toSeq
}

abstract class UnitWithJob[T <: WrapsUnit](val employer: Employer[T], val unit: T, val priority: Priority)
  extends HasUniverse {
  override val universe     = employer.universe
  private  val creationTick = currentTick
  private  val listeners    = ArrayBuffer.empty[JobFinishedListener[T]]
  def age = currentTick - creationTick
  def isIdle: Boolean = false
  def ordersForTick: Seq[UnitOrder]
  override def toString: String = s"${getClass.className} of $unit of $employer"
  def isFinished: Boolean
  def onFinish(): Unit = {
    listeners.foreach(_.onFinish())
  }
  def listen_!(listener: JobFinishedListener[T]): Unit = listeners += listener
}

trait HasFunding[T <: WrapsUnit] extends UnitWithJob[T] with HasUniverse {

  listen_!(() => release_!())

  def proofForFunding: ResourceApprovalSuccess

  private def release_!(): Unit = {
    resources.unlock_!(proofForFunding)
  }
}

trait JobFinishedListener[T <: WrapsUnit] {
  def onFinish(): Unit
}

class TrainUnit[F <: Factory, T <: Mobile](factory: F, trainType: Class[_ <: T], employer: Employer[F],
                                           funding: ResourceApprovalSuccess)
  extends UnitWithJob[F](employer, factory, Priority.Default) with HasFunding[F] {

  override def proofForFunding = funding

  override def ordersForTick: Seq[UnitOrder] = {
    Orders.Train(unit, trainType).toSeq
  }

  override def isFinished = {
    !factory.isProducing && age > 10
  }
}

class ConstructBuilding[W <: WorkerUnit, B <: Building](worker: W, buildingType: Class[_ <: B], employer: Employer[W],
                                                        where: MapTilePosition, funding: ResourceApprovalSuccess)
  extends UnitWithJob[W](employer, worker, Priority.ConstructBuilding) with HasFunding[W] {

  private var startedConstruction  = false
  private var finishedConstruction = false
  def typeOfBuilding = buildingType

  override def proofForFunding = funding

  override def ordersForTick: Seq[UnitOrder] = {
    Orders.Construct(unit, buildingType, where).toSeq
  }
  override def isFinished = {
    if (!startedConstruction) {
      startedConstruction = worker.isConstructing
    }
    if (startedConstruction) {
      finishedConstruction = !worker.isConstructing
    }
    age > 50 && startedConstruction && finishedConstruction
  }
}

class BusyDoingNothing[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.None) {
  override def isIdle = true
  override def ordersForTick: Seq[UnitOrder] = Nil
  override def isFinished = false
}

class BusyBeingTrained[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.Max) {
  override def isIdle = false
  override def ordersForTick: Seq[UnitOrder] = Nil
  override def isFinished = unit.nativeUnit.getRemainingBuildTime == 0
}

trait UnitRequest[T <: WrapsUnit] {
  def includesByType(unit: WrapsUnit): Boolean = typeOfRequestedUnit.isAssignableFrom(unit.getClass)

  def typeOfRequestedUnit: Class[_ <: T]
  def amount: Int
  def acceptable(unit: T) = true
  def cherryPicker: Option[(T, T) => T] = None

  override def toString = s"UnitRequest($typeOfRequestedUnit, $amount)"
}

case class AnyUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[_ <: T], amount: Int) extends UnitRequest[T]

case class AnyFactoryRequest[T <: Factory, U <: Mobile](typeOfRequestedUnit: Class[_ <: T], amount: Int,
                                                        buildThis: Class[_ <: U]) extends UnitRequest[T] {
  override def acceptable(unit: T): Boolean = {
    unit.canBuild(buildThis)
  }
}

case class BuildUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[_ <: T], amount: Int) extends UnitRequest[T]

case class SpecificUnitRequest[T <: WrapsUnit](unit: T) extends UnitRequest[T] {
  override def typeOfRequestedUnit = unit.getClass
  override def amount: Int = 1
}

case class UnitJobRequests[T <: WrapsUnit : Manifest](requests: Seq[UnitRequest[T]], employer: Employer[T],
                                                      priority: Priority) {
  private val types = requests.map(_.typeOfRequestedUnit).toSet

  def wantsUnit(unitType: WrapsUnit) = types.exists(_.isAssignableFrom(unitType.getClass))

  def commonUnitType = manifest[T].runtimeClass
}

object UnitJobRequests {
  def builderOf[T <: Mobile, F <: Factory : Manifest](wantedType: Class[_ <: T],
                                                      employer: Employer[F]): UnitJobRequests[F] = {

    val actualClass = manifest[F].runtimeClass.asInstanceOf[Class[F]]
    val req = AnyFactoryRequest[F, T](actualClass, 1, wantedType)

    UnitJobRequests(req.toSeq, employer, Priority.Default)

  }

  def constructor[T <: WorkerUnit : Manifest](employer: Employer[T]): UnitJobRequests[T] = {

    val actualClass = manifest[T].runtimeClass.asInstanceOf[Class[T]]
    val req = AnyUnitRequest(actualClass, 1)

    UnitJobRequests(req.toSeq, employer, Priority.ConstructBuilding)
  }

  def idleOfType[T <: WrapsUnit : Manifest](employer: Employer[T], ofType: Class[_ <: T], amount: Int = 1) = {
    val req: AnyUnitRequest[T] = AnyUnitRequest(ofType, amount)

    UnitJobRequests[T](req.toSeq, employer, Priority.Default)
  }

  def buildingOfType[T <: Building : Manifest](employer: Employer[T], ofType: Class[_ <: T], amount: Int = 1) = {
    val req: AnyUnitRequest[T] = AnyUnitRequest(ofType, amount)

    UnitJobRequests[T](req.toSeq, employer, Priority.Default)
  }

  def newOfType[T <: WrapsUnit : Manifest](employer: Employer[T], ofType: Class[_ <: T], amount: Int = 1) = {
    val req: BuildUnitRequest[T] = BuildUnitRequest(ofType, amount)

    UnitJobRequests[T](req.toSeq, employer, Priority.Default)
  }

}









