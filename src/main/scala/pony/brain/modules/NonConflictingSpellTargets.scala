package pony
package brain
package modules

import scala.collection.mutable
import scala.reflect.ClassTag

object NonConflictingSpellTargets {
  def forSpell[T <: HasSingleTargetSpells, M <: Mobile: ClassTag](
      spell: SingleTargetSpell[T, M],
      universe: Universe
  ) = {

    new NonConflictingSpellTargets(
      spell,
      {
        case x: M if spell.canBeCastOn(x) & spell.shouldActivateOn(x) => x
      },
      spell.isAffected,
      universe
    )
  }
}

class NonConflictingSpellTargets[T <: HasSingleTargetSpells, M <: Mobile: ClassTag](
    spell: SingleTargetSpell[T, M],
    targetConstraint: PartialFunction[Mobile, M],
    keepLocked: M => Boolean,
    override val universe: Universe
) extends HasUniverse {
  private val locked = mutable.HashSet.empty[M]

  private val lockedTargets      = mutable.HashSet.empty[Target[M]]
  private val assignments        = mutable.HashMap.empty[M, Target[M]]
  private val prioritizedTargets = LazyVal.from {
    val base = {
      val targets = {
        if (spell.castOn == EnemyUnits) {
          universe.enemyUnits.allByType[M]
        } else {
          universe.ownUnits.allByType[M]
        }
      }
      targets.collect(targetConstraint)
    }

    spell.priorityRule.fold(base.toVector) { rule =>
      base.toVector.sortBy(m => -rule(m))
    }
  }

  def afterTick(): Unit = {
    prioritizedTargets.invalidate()
    locked.filterNot(keepLocked).foreach { elem =>
      unlock_!(elem)
    }
  }

  private def unlock_!(target: M): Unit = {
    locked -= target
    val old = assignments.remove(target).get
    lockedTargets -= old
    prioritizedTargets.invalidate()
  }

  def notifyLock_!(t: T, target: M): Unit = {
    locked += target
    val tar = Target(t, target)
    lockedTargets += tar
    assignments.put(target, tar)
    prioritizedTargets.invalidate()
  }

  def suggestTargetFor(caster: T): Option[M] = {
    // for now, just pick the first in range that is not yet taken
    val range = spell.castRangeSquare

    val filtered = prioritizedTargets.filterNot(locked)
    filtered.find { _.currentPosition.distanceSquaredTo(caster.currentPosition) < range }
  }
}
