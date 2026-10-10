package pony
package brain
package modules
package micro

import pony.units.{Mobile, WrapsUnit}

import scala.collection.mutable
import scala.reflect.ClassTag

class NonConflictingTargets[T <: WrapsUnit: ClassTag, M <: Mobile: ClassTag](
    override val universe: Universe,
    rateTarget: T => PriorityChain,
    validTargetTest: T => Boolean,
    subAccept: (M, T) => Boolean,
    subRate: (M, T) => PriorityChain,
    own: Boolean,
    allowReplacements: Boolean
) extends HasUniverse {

  private val validTarget        = (t: T) => t.isInGame && validTargetTest(t)
  private val locks              = mutable.HashSet.empty[T]
  private val assignments        = mutable.HashMap.empty[M, T]
  private val assignmentsReverse = mutable.HashMap.empty[T, M]
  private val targets            = universe.oncePerTick {
    val on = if (own) ownUnits else enemies
    on.allByType[T].iterator
      .filter(validTarget)
      .map { e => e -> rateTarget(e) }
      .toVector
      .sortBy(_._2)
      .map(_._1)
  }

  override def onTick_!() = {
    super.onTick_!()
    val noLongerValid = assignments.filter { case (m, t) =>
      !m.isInGame || !validTarget(t)
    }
    noLongerValid.foreach { case (k, v) => unlock_!(k, v) }
  }

  def suggestTarget(m: M) = {
    assignments.get(m) match {
      case x @ Some(target) =>
        if (validTarget(target))
          x
        else {
          unlock_!(m, target)
          None
        }
      case None =>
        val newSuggestion = targets.get
          .iterator
          .filterNot(locked)
          .filter(subAccept(m, _))
          .maxByOpt(subRate(m, _))
        newSuggestion match {
          case Some(t) =>
            lock_!(t, m)
            newSuggestion
          case None =>
            if (allowReplacements) {
              // a locked target may have become invalid since (a Comsat whose position turned unknown made the
              // repair acceptance throw in game 2 on 85e02a3)
              val bestToReplace =
                locks
                  .iterator
                  .filter(validTarget)
                  .filter(subAccept(m, _))
                  .maxByOpt(subRate(m, _))

              bestToReplace match {
                case Some(maybeStealMe) =>
                  val lockedOn                     = assignmentsReverse(maybeStealMe)
                  val ord: Ordering[PriorityChain] = implicitly
                  if (ord.lt(subRate(lockedOn, maybeStealMe), subRate(m, maybeStealMe))) {
                    unlock_!(lockedOn, maybeStealMe)
                    lock_!(maybeStealMe, m)
                    Some(maybeStealMe)
                  } else {
                    None
                  }
                case None => None
              }
            } else {
              None
            }

        }
    }
  }

  def unlock_!(m: M, target: T): Unit = {
    assignments.remove(m)
    assignmentsReverse.remove(target)
    locks -= target
  }

  def lock_!(t: T, m: M): Unit = {
    assert(!locked(t))
    assert(!assignments.contains(m))

    assignments.put(m, t)
    assignmentsReverse.put(t, m)
    locks += t
  }

  private def locked(t: T) = locks(t)

  def unlock_!(m: M): Unit = {
    if (assignments.contains(m)) {
      val target = assignments(m)
      unlock_!(m, target)
    }
  }

  private def targetOf(m: M) = assignments(m)
}
