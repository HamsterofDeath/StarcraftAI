package pony
package brain

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class UnitManager(universe: Universe) {
  private val assignments = mutable.HashMap.empty[WrapsUnit, UnitWithJob]
  private val byEmployer  = new
      mutable.HashMap[Employer, mutable.Set[UnitWithJob]] with mutable.MultiMap[Employer, UnitWithJob]
  def assignJob(employer: Employer, newJob: UnitWithJob): Unit = {
    assignments.get(newJob.unit).foreach { oldJob =>
      val oldAssignment = byEmployer.find(_._2(oldJob))
      oldAssignment.foreach { case (oldEmployer, _) =>
        byEmployer.removeBinding(oldEmployer, oldJob)
      }
    }

    assignments.put(newJob.unit, newJob)
    byEmployer.addBinding(employer, newJob)

  }
  def jobOf(unit: WrapsUnit) = assignments(unit)

  def tick(): Unit = {
    val myOwn = universe.world.units.mine.filterNot(assignments.contains).map {new BusyDoingNothing(_, Nobody)}.toSeq
    myOwn.foreach(byEmployer.addBinding(Nobody, _))
    assignments ++= myOwn.map(e => e.unit -> e)

    val registerUs = universe.world.units.all.filterNot(assignments.contains).map {new BusyDoingNothing(_, Nobody)}
    assignments ++= registerUs.map(e => e.unit -> e)
  }
  def request(req: UnitJobRequests) = {
    def result: Option[UnitCollector] = {
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
      case None => FailedHiringResult
      case Some(team) if team.complete => new SuccessfulHiringResult(team.teamAsMap)
      case Some(team) => new PartialHiringResult(team.teamAsMap)
    }
  }
  private def allOfEmployer(employer: Employer) = byEmployer.getOrElse(employer, Set.empty)
  private def allNotOfEmployer(employer: Employer) = byEmployer.filter(_._1 != employer).flatMap(_._2)

  case object Nobody extends Employer(universe.units)
}

trait HiringResult {
  def success: Boolean
  def hired: Map[UnitRequest, Set[WrapsUnit]]
  def units = hired.flatMap(_._2)
}

object FailedHiringResult extends HiringResult {
  def success = false
  def hired = Map.empty
}

class SuccessfulHiringResult(override val hired: Map[UnitRequest, Set[WrapsUnit]]) extends HiringResult {
  def success = true
}

class PartialHiringResult(override val hired: Map[UnitRequest, Set[WrapsUnit]]) extends HiringResult {
  def success = false
}

class UnitCollector(request: UnitJobRequests) {
  private val hired                      = new
      mutable.HashMap[UnitRequest, mutable.Set[WrapsUnit]] with mutable.MultiMap[UnitRequest, WrapsUnit]
  private val remainingAmountsPerRequest = mutable.HashMap.empty ++ request.requests.map { e => e -> e.amount }.toMap

  def teamAsMap = hired.toSeq.map { case (k, v) => k -> v.toSet }.toMap

  def collect(unit: WrapsUnit) = {
    remainingAmountsPerRequest.find {
      case (req, remaining) if remaining > 0 && req.includesByType(unit) => true
      case _ => false
    }.foreach { case (req, missing) =>
      remainingAmountsPerRequest.put(req, missing - 1)
      hired.addBinding(req, unit)
    }
  }

  def complete = remainingAmountsPerRequest.valuesIterator.sum == 0

  def requests(unit: UnitWithJob) = {
    request.wantsUnit(unit.unit)
  }
}

abstract class Employer(unitManager: UnitManager) {
  private var employees = ArrayBuffer.empty[WrapsUnit]

  def teamSize = employees.size

  def idleUnits = employees.filter(unitManager.jobOf(_).isIdle)

  def hire(result: HiringResult): Unit = {
    result.units.foreach(hire)
  }

  def hire(unit: WrapsUnit): Unit = {
    employees += unit
  }

  def fire(unit: WrapsUnit): Unit = {
    employees -= unit
  }

  def assignJob(job: UnitWithJob): Unit = {
    assert(employees.contains(job.unit))
    unitManager.assignJob(this, job)
  }

  def current = employees.toSeq
}

abstract class UnitWithJob(val employer: Employer, val unit: WrapsUnit, val priority: Priority) {
  def isIdle: Boolean = false
  def ordersForTick: Seq[Order]
  override def toString: String = s"${getClass.getSimpleName} of ${unit} of ${employer}"
}

class BusyDoingNothing(unit: WrapsUnit, employer: Employer) extends UnitWithJob(employer, unit, Priority.None) {
  override def isIdle = true
  override def ordersForTick: Seq[Order] = Nil
}

trait UnitRequest {
  def includesByType(unit: WrapsUnit): Boolean = typeOfRequestedUnit.isAssignableFrom(unit.getClass)

  def typeOfRequestedUnit: SCUnitType
  def amount: Int
  def acceptable(unit: WrapsUnit) = true
  def cherryPicker: Option[(WrapsUnit, WrapsUnit) => WrapsUnit] = None
}

case class AnyUnitRequest(typeOfRequestedUnit: SCUnitType, amount: Int) extends UnitRequest

case class SpecificUnitRequest(unit: WrapsUnit) extends UnitRequest {
  override def typeOfRequestedUnit = unit.getClass
  override def amount: Int = 1
}

case class UnitJobRequests(requests: Seq[UnitRequest], employer: Employer, priority: Priority) {
  private val types = requests.map(_.typeOfRequestedUnit).toSet

  def wantsUnit(unitType: WrapsUnit) = types.exists(_.isAssignableFrom(unitType.getClass))
}

object UnitJobRequests {
  def idleOfType(employer: Employer, ofType: SCUnitType, amount: Int = 1) = {
    val req = AnyUnitRequest(ofType, amount)

    UnitJobRequests(Seq(req), employer, Priority.Default)
  }

}









