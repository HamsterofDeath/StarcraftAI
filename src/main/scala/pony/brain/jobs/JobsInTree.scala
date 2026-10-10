package pony
package brain
package jobs

import pony.units.WrapsUnit

import scala.collection.mutable

class JobsInTree {
  private val flat       = multiMap[Employer[? <: WrapsUnit], UnitWithJob[? <: WrapsUnit]]
  private val indexBy    = mutable.HashSet.empty[JobIndex[? <: WrapsUnit]]
  private val byEmployer = mutable.HashMap
    .empty[JobIndex[? <: WrapsUnit], JobsByClass[? <: WrapsUnit]]

  def allNotOfEmployer[T <: WrapsUnit](employer: Employer[T]) = {
    allFlat.filter(_._1 != employer).flatMap(_._2)
  }

  def allFlat = flat

  def jobsOf[T <: WrapsUnit](employer: Employer[T], unitType: Class[? <: T]) = {
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

  def getTyped[T <: WrapsUnit](c: JobIndex[? <: WrapsUnit]): JobsByClass[T] = {
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
