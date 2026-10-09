package pony
package brain

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

// TODO check if this class really has a purpose
class Employer[T <: WrapsUnit: ClassTag](override val universe: Universe) extends HasUniverse {
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
    assert(
      !employees.contains(job.unit),
      s"Already hired ${job.unit} by $this, cannot give it new job $job because it already has ${
          unitManager.jobOf(job.unit)
        }"
    )
    assert(this == job.employer, s"$this is not ${job.employer}")
    unitManager.assignJob_!(job)
  }

  def current = employees.immutableView
}
