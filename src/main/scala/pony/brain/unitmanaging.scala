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
  def failedToProvide = unfulfilledRequests.toSeq
  def assignJob[T <: WrapsUnit](employer: Employer[T], newJob: UnitWithJob[T]): Unit = {
    assignments.get(newJob.unit).foreach { oldJob =>
      val oldAssignment = byEmployer.find(_._2(oldJob))
      oldAssignment.foreach { case (oldEmployer, _) =>
        byEmployer.removeBinding(oldEmployer, oldJob)
      }
    }

    assignments.put(newJob.unit, newJob)
    byEmployer.addBinding(employer, newJob)

  }
  def jobOf[T <: WrapsUnit](unit: T) = assignments(unit).asInstanceOf[UnitWithJob[T]]

  def tick(): Unit = {
    // do not pile these up, clear per tick
    unfulfilledRequests.clear()

    val myOwn = universe.world.units.mine.filterNot(assignments.contains).map {new BusyDoingNothing(_, Nobody)}.toSeq
    myOwn.foreach(byEmployer.addBinding(Nobody, _))
    assignments ++= myOwn.map(e => e.unit -> e)

    val registerUs = universe.world.units.all.filterNot(assignments.contains).map {new BusyDoingNothing(_, Nobody)}
    assignments ++= registerUs.map(e => e.unit -> e)
  }

  def request[T <: WrapsUnit : Manifest](req: UnitJobRequests[T]) = {

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

    hireResult match {
      case None =>
        unfulfilledRequests += req
        new FailedHiringResult[T]
      case Some(team) if !team.complete =>
        unfulfilledRequests += team.missingAsRequest
        new PartialHiringResult(team.teamAsMap)
      case Some(team) =>
        new SuccessfulHiringResult(team.teamAsMap)
    }
  }
  private def allOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.getOrElse(employer, Set.empty)
  private def allNotOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.filter(_._1 != employer)
                                                                        .flatMap(_._2)

  case object Nobody extends Employer[WrapsUnit](universe)
}

trait HiringResult[T <: WrapsUnit] {
  def success: Boolean
  def hired: Map[UnitRequest[T], Set[T]]
  def units = hired.flatMap(_._2)
}

class FailedHiringResult[T <: WrapsUnit] extends HiringResult[T] {
  def success = false
  def hired = Map.empty
}

trait AtLeastOneSuccess[T <: WrapsUnit] extends HiringResult[T] {
  def one = hired.valuesIterator.next().head
}

class SuccessfulHiringResult[T <: WrapsUnit](override val hired: Map[UnitRequest[T], Set[T]])
  extends HiringResult[T] with AtLeastOneSuccess[T] {
  def success = true
}

class PartialHiringResult[T <: WrapsUnit](override val hired: Map[UnitRequest[T], Set[T]])
  extends HiringResult[T] with AtLeastOneSuccess[T] {
  def success = false
}

class UnitCollector[T <: WrapsUnit : Manifest](originalRequest: UnitJobRequests[T]) {
  private val hired                      = new
      mutable.HashMap[UnitRequest[T], mutable.Set[T]] with mutable.MultiMap[UnitRequest[T], T]
  private val remainingAmountsPerRequest = mutable.HashMap.empty ++
                                           originalRequest.requests.map { e => e -> e.amount }.toMap
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

abstract class Employer[T <: WrapsUnit : Manifest](override val universe: Universe) extends HasUniverse {

  private var employees = ArrayBuffer.empty[T]

  def teamSize = employees.size

  def idleUnits = employees.filter(units.jobOf(_).isIdle)

  def hire(result: HiringResult[T]): Unit = {
    result.units.foreach(hire)
  }

  def hire(unit: T): Unit = {
    employees += unit
  }

  def fire(unit: T): Unit = {
    employees -= unit
  }

  def assignJob(job: UnitWithJob[T]): Unit = {
    assert(employees.contains(job.unit))
    units.assignJob(this, job)
  }

  def current = employees.toSeq
}

abstract class UnitWithJob[T <: WrapsUnit](val employer: Employer[T], val unit: T, val priority: Priority) {
  def isIdle: Boolean = false
  def ordersForTick: Seq[UnitOrder]
  override def toString: String = s"${getClass.getSimpleName} of ${unit} of ${employer}"
}

class TrainUnit[F <: Factory, T <: Mobile](unit: F, trainType: Class[_ <: Mobile], employer: Employer[F])
  extends UnitWithJob[F](employer, unit, Priority.Default) {

  override def ordersForTick: Seq[UnitOrder] = {
    Orders.Train(unit, trainType).toSeq
  }
}

class BusyDoingNothing[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.None) {
  override def isIdle = true
  override def ordersForTick: Seq[UnitOrder] = Nil
}

trait UnitRequest[T <: WrapsUnit] {
  def includesByType(unit: WrapsUnit): Boolean = typeOfRequestedUnit.isAssignableFrom(unit.getClass)

  def typeOfRequestedUnit: Class[_ <: T]
  def amount: Int
  def acceptable(unit: T) = true
  def cherryPicker: Option[(T, T) => T] = None
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

  def idleOfType[T <: WrapsUnit : Manifest](employer: Employer[T], ofType: Class[T], amount: Int = 1) = {
    val req = AnyUnitRequest(ofType, amount)

    UnitJobRequests[T](req.toSeq, employer, Priority.Default)
  }

  def newOfType[T <: WrapsUnit : Manifest](employer: Employer[T], ofType: Class[T], amount: Int = 1) = {
    val req = BuildUnitRequest(ofType, amount)

    UnitJobRequests[T](req.toSeq, employer, Priority.Default)
  }

}









