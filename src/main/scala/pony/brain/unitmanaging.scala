package pony
package brain

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class UnitManager(universe: Universe) {
  private val assignments = mutable.HashMap.empty[WrapsUnit, UnitWithJob[_ <: WrapsUnit]]

  private val byEmployer  = new
      mutable.HashMap[Employer[_ <: WrapsUnit], mutable.Set[UnitWithJob[_ <: WrapsUnit]]]
      with mutable.MultiMap[Employer[_ <: WrapsUnit], UnitWithJob[_ <: WrapsUnit]]

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
    val myOwn = universe.world.units.mine.filterNot(assignments.contains).map {new BusyDoingNothing(_, Nobody)}.toSeq
    myOwn.foreach(byEmployer.addBinding(Nobody, _))
    assignments ++= myOwn.map(e => e.unit -> e)

    val registerUs = universe.world.units.all.filterNot(assignments.contains).map {new BusyDoingNothing(_, Nobody)}
    assignments ++= registerUs.map(e => e.unit -> e)
  }
  def request[T <: WrapsUnit](req: UnitJobRequests[T]) = {
    def result: Option[UnitCollector[T]] = {
      val available = (allOfEmployer(Nobody) ++ allNotOfEmployer(Nobody)).filter(_.priority < req.priority)
      val collector = new UnitCollector(req)
      available.foreach { candidate =>
        if (collector.requests(candidate)) {
          collector.collect(candidate.unit)
        }

        if (collector.complete) {
          return Some(collector)
        }
      }
      None
    }
    result match {
      case None => new FailedHiringResult[T]
      case Some(team) if team.complete => new SuccessfulHiringResult(team.teamAsMap)
      case Some(team) => new PartialHiringResult(team.teamAsMap)
    }
  }
  private def allOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.getOrElse(employer, Set.empty)
  private def allNotOfEmployer[T <: WrapsUnit](employer: Employer[T]) = byEmployer.filter(_._1 != employer)
                                                                        .flatMap(_._2)

  case object Nobody extends Employer[WrapsUnit](universe.units)
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

class SuccessfulHiringResult[T <: WrapsUnit](override val hired: Map[UnitRequest[T], Set[T]]) extends HiringResult[T] {
  def success = true
}

class PartialHiringResult[T <: WrapsUnit](override val hired: Map[UnitRequest[T], Set[T]]) extends HiringResult[T] {
  def success = false
}

class UnitCollector[T <: WrapsUnit](request: UnitJobRequests[T]) {
  private val hired                      = new
      mutable.HashMap[UnitRequest[T], mutable.Set[T]] with mutable.MultiMap[UnitRequest[T], T]
  private val remainingAmountsPerRequest = mutable.HashMap.empty ++ request.requests.map { e => e -> e.amount }.toMap

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
    request.wantsUnit(unit.unit)
  }
}

abstract class Employer[T <: WrapsUnit](unitManager: UnitManager) {
  private var employees = ArrayBuffer.empty[T]

  def teamSize = employees.size

  def idleUnits = employees.filter(unitManager.jobOf(_).isIdle)

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
    unitManager.assignJob(this, job)
  }

  def current = employees.toSeq
}

abstract class UnitWithJob[T <: WrapsUnit](val employer: Employer[T], val unit: T, val priority: Priority) {
  def isIdle: Boolean = false
  def ordersForTick: Seq[Order]
  override def toString: String = s"${getClass.getSimpleName} of ${unit} of ${employer}"
}

class BusyDoingNothing[T <: WrapsUnit](unit: T, employer: Employer[T])
  extends UnitWithJob(employer, unit, Priority.None) {
  override def isIdle = true
  override def ordersForTick: Seq[Order] = Nil
}

trait UnitRequest[T <: WrapsUnit] {
  def includesByType(unit: WrapsUnit): Boolean = typeOfRequestedUnit.isAssignableFrom(unit.getClass)

  def typeOfRequestedUnit: SCUnitType
  def amount: Int
  def acceptable(unit: T) = true
  def cherryPicker: Option[(T, T) => T] = None
}

case class AnyUnitRequest[T <: WrapsUnit](typeOfRequestedUnit: Class[T], amount: Int) extends UnitRequest[T]

case class SpecificUnitRequest[T <: WrapsUnit](unit: T) extends UnitRequest[T] {
  override def typeOfRequestedUnit = unit.getClass
  override def amount: Int = 1
}

case class UnitJobRequests[T <: WrapsUnit](requests: Seq[UnitRequest[T]], employer: Employer[T], priority: Priority) {
  private val types = requests.map(_.typeOfRequestedUnit).toSet

  def wantsUnit(unitType: WrapsUnit) = types.exists(_.isAssignableFrom(unitType.getClass))
}

object UnitJobRequests {
  def idleOfType[T <: WrapsUnit](employer: Employer[T], ofType: Class[T], amount: Int = 1) = {
    val req = AnyUnitRequest(ofType, amount)

    UnitJobRequests[T](req.toSeq, employer, Priority.Default)
  }

}









