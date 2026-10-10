package pony
package brain
package modules
package micro

import pony.combat.{EnemyUnits, HasSingleTargetSpells, SingleTargetSpell}
import pony.units.Mobile
import pony.util.LazyVal

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

/**
  * Hands each caster a target no other caster is already working on. Units the spell already affects are no targets;
  * a target stays taken while a cast is on its way to it, at most `InFlightTicks`, so a lost order frees it again.
  */
class NonConflictingSpellTargets[T <: HasSingleTargetSpells, M <: Mobile: ClassTag](
    spell: SingleTargetSpell[T, M],
    targetConstraint: PartialFunction[Mobile, M],
    affected: M => Boolean,
    override val universe: Universe
) extends HasUniverse {
  private val InFlightTicks = 48

  /** Targets of casts on their way, with the tick each was ordered. */
  private val locked = mutable.HashMap.empty[M, Int]

  private val prioritizedTargets = LazyVal.from {
    val base = {
      val targets = {
        if (spell.castOn == EnemyUnits) {
          universe.enemyUnits.allByType[M]
        } else {
          universe.ownUnits.allByType[M]
        }
      }
      targets.collect(targetConstraint).filterNot(affected)
    }

    spell.priorityRule.fold(base.toVector) { rule =>
      base.toVector.sortBy(m => -rule(m))
    }
  }

  def afterTick(): Unit = {
    prioritizedTargets.invalidate()
    val now = universe.currentTick
    locked.filterInPlace((target, since) => !affected(target) && now - since < InFlightTicks)
  }

  def notifyLock_!(t: T, target: M): Unit = {
    locked.put(target, universe.currentTick)
    prioritizedTargets.invalidate()
  }

  /** The best untaken target within cast range of the caster. */
  def suggestTargetFor(caster: T): Option[M] = {
    val range = spell.castRangeSquare
    prioritizedTargets.get.iterator.filterNot(locked.contains).find {
      _.currentPosition.distanceSquaredTo(caster.currentPosition) < range
    }
  }
}
