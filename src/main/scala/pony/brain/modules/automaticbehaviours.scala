package pony.brain.modules

import pony.brain.{HasUniverse, Objective, SingleUnitBehaviour, Universe}
import pony.{CanCloak, CanDie, CanUseStimpack, DamageSingleAttack, Ghost, HasSingleTargetSpells, InstantAttack,
LazyVal, Medic, Mobile, MobileDetector, MobileRangeWeapon, Orders, SCV, SingleTargetSpell, Spells, SupportUnit,
Upgrades, Vulture, WrapsUnit}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class DefaultBehaviour[T <: Mobile : Manifest](override val universe: Universe) extends HasUniverse {
  private val controlled      = new
      collection.mutable.HashMap[Objective, collection.mutable.Set[SingleUnitBehaviour[T]]]
      with mutable.MultiMap[Objective, SingleUnitBehaviour[T]]
  private val unit2behaviour  = mutable.HashMap.empty[T, SingleUnitBehaviour[T]]
  private val controlledUnits = mutable.HashSet.empty[T]
  def behaviourOf(unit: Mobile) = {
    ifControlsOpt(unit) {identity}
  }
  def ifControlsOpt[R](m: Mobile)(f: (SingleUnitBehaviour[T]) => R) = {
    ifControls(m, Option.empty[R])(e => Some(f(e)))
  }
  def ifControls[R](m: Mobile, ifNot: R)(f: (SingleUnitBehaviour[T]) => R) = {
    if (controls(m)) {
      f(unit2behaviour(assumeSafe(m)))
    } else {
      ifNot
    }
  }
  def controls(unit: Mobile) = {
    canControl(unit) && controlledUnits.contains(assumeSafe(unit))
  }
  def assumeSafe(unit: Mobile): T = unit.asInstanceOf[T]
  def canControl(u: WrapsUnit) = {
    manifest[T].runtimeClass.isAssignableFrom(u.getClass)
  }
  def add_!(u: WrapsUnit, objective: Objective) = {
    assert(canControl(u))
    val behaviour = lift(u.asInstanceOf[T])
    controlled.addBinding(objective, behaviour)
    controlledUnits += behaviour.unit
    unit2behaviour.put(behaviour.unit, behaviour)
  }
  def cast = this.asInstanceOf[DefaultBehaviour[Mobile]]

  protected def lift(t: T): SingleUnitBehaviour[T]

  def onTick(): Unit = {}
}

object Terran {
  def allBehaviours(universe: Universe): Seq[DefaultBehaviour[Mobile]] = {
    val allOfThem = (
                      new StimSelf(universe) ::
                      new StopMechanic(universe) ::
                      /*
                                            new RepairDamagedBuilding ::
                                            new RepairDamagedUnit ::
                                           new RevealHiddenUnitsPermanenly ::
                                           new SetupMineField ::
                                           new Scout ::
                                           new Cloak ::
                                           new DoNotStray ::
                                           new HealDamagedUnit ::
                                           new FixMedicalProblem ::
                                           new BlindDetector ::
                                           new Evade ::
                                           new StopToFire ::
                                           new FocusFire ::
                      */
                      Nil).map(_.cast)
    allOfThem
  }

  class RepairDamagedBuilding(universe: Universe) extends DefaultBehaviour[SCV](universe) {
    override protected def lift(t: SCV): SingleUnitBehaviour[SCV] = ???
  }
  class RepairDamagedUnit(universe: Universe) extends DefaultBehaviour[SCV](universe) {
    override protected def lift(t: SCV): SingleUnitBehaviour[SCV] = ???
  }

  class StimSelf(universe: Universe) extends DefaultBehaviour[CanUseStimpack](universe) {
    override protected def lift(t: CanUseStimpack) = new SingleUnitBehaviour[CanUseStimpack](t) {

      override def preconditionOk = upgrades.hasResearched(Upgrades.Terran.InfantryCooldown)

      override def toOrder(what: Objective) = {
        if (t.isAttacking && !t.isStimmed) {
          List(Orders.Stimpack(t))
        } else {
          Nil
        }
      }
      override def shortName: String = s"Stim"
    }
  }
  class RevealHiddenUnitsPermanenly(universe: Universe) extends DefaultBehaviour[MobileDetector](universe) {
    override protected def lift(t: MobileDetector): SingleUnitBehaviour[MobileDetector] = ???
  }
  class SetupMineField(universe: Universe) extends DefaultBehaviour[Vulture](universe) {
    override protected def lift(t: Vulture): SingleUnitBehaviour[Vulture] = ???
  }
  class Scout(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override protected def lift(t: Mobile): SingleUnitBehaviour[Mobile] = ???
  }
  class Cloak(universe: Universe) extends DefaultBehaviour[CanCloak](universe) {
    override protected def lift(t: CanCloak): SingleUnitBehaviour[CanCloak] = ???
  }
  class DoNotStray(universe: Universe) extends DefaultBehaviour[SupportUnit](universe) {
    override protected def lift(t: SupportUnit): SingleUnitBehaviour[SupportUnit] = ???
  }

  class StopMechanic(universe: Universe) extends DefaultBehaviour[Ghost](universe) {
    private val helper = NonConflictingTargetPicks.forSpell(Spells.Lockdown, universe)

    override protected def lift(t: Ghost): SingleUnitBehaviour[Ghost] = new SingleUnitBehaviour(t) {
      private def spell = Upgrades.Terran.GhostStop
      override def preconditionOk = upgrades.hasResearched(spell)

      override def shortName = "Lockdown"

      override def toOrder(what: Objective) = {
        if (t.canCastNow(spell)) {
          val h = helper
          h.suggestTargetFor(t).map { target =>
            h.notifyLock_!(t, target)
            t.toOrder(spell, target)
          }.toList
        } else {
          Nil
        }
      }

    }
  }
  class HealDamagedUnit(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class FixMedicalProblem(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class BlindDetector(universe: Universe) extends DefaultBehaviour[Medic](universe) {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class Evade(universe: Universe) extends DefaultBehaviour[Mobile](universe) {
    override protected def lift(t: Mobile): SingleUnitBehaviour[Mobile] = ???
  }
  class StopToFire(universe: Universe) extends DefaultBehaviour[InstantAttack](universe) {
    override protected def lift(t: InstantAttack): SingleUnitBehaviour[InstantAttack] = ???
  }
  class FocusFire(universe: Universe) extends DefaultBehaviour[MobileRangeWeapon](universe) {

    private val helper = new FocusFireOrganizer(universe)

    override protected def lift(t: MobileRangeWeapon): SingleUnitBehaviour[MobileRangeWeapon] = new
        SingleUnitBehaviour(t) {
      override def shortName = "Focus fire"
      override def toOrder(what: Objective) = {
        helper.suggestTarget(t).map { target =>
          Orders.AttackUnit(t, target)
        }.toList
      }
    }
  }
}

case class Target[T <: Mobile](caster: HasSingleTargetSpells, target: T)

class FocusFireOrganizer(override val universe: Universe) extends HasUniverse {

  class Attackers(val target: CanDie) {

    def isAttacker(t: MobileRangeWeapon) = attackers(t)

    def addAttacker_!(t: MobileRangeWeapon): Unit = {
      assert(!attackers(t), s"$attackers already contains $t")
      attackers += t
      val damageDone = t.calculateDamageOn(target.armor)
      plannedDamage += damageDone
      hpAfterNextAttacks -! damageDone
    }

    class NormalizedHP(var hp: Int, var shields: Int) {
      assert(shields >= 0)

      def alive = hp > 0

      def -!(dsa: DamageSingleAttack) = {
        hp -= dsa.onHp
        shields -= dsa.onShields
      }
    }

    private val attackers          = mutable.HashSet.empty[MobileRangeWeapon]
    private val plannedDamage      = ArrayBuffer.empty[DamageSingleAttack]
    private var normalizedActualHP = target.hitPoints
    private var hpAfterNextAttacks = new
        NormalizedHP(normalizedActualHP.hitpoints, normalizedActualHP.shield)

    def canTakeMore = hpAfterNextAttacks.alive
  }

  private val assignments = mutable.HashMap.empty[CanDie, Attackers]

  def suggestTarget(t: MobileRangeWeapon): Option[CanDie] = {
    universe.enemyUnits.allCanDie.find { target =>
      val existing = assignments.get(target).exists(_.isAttacker(t))
      existing || (t.isInWeaponRange(target) && t.canAttack(target) &&
                   assignments.getOrElseUpdate(target, new Attackers(target)).canTakeMore)
    }.foreach { attackThis =>
      val plan = assignments(attackThis)
      plan.addAttacker_!(t)
    }
    assignments.get(t).map(_.target)
  }
}

object NonConflictingTargetPicks {
  def forSpell[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell: SingleTargetSpell[T, M],
                                                                   universe: Universe) = {

    new NonConflictingTargetPicks(spell, { case x: Mobile if spell.canBeCastOn(x) => x.asInstanceOf[M] },
    spell.isAffected, universe)
  }
}

class NonConflictingTargetPicks[T <: HasSingleTargetSpells, M <: Mobile : Manifest](spell: SingleTargetSpell[T, M],
                                                                                    targetConstraint:
                                                                                    PartialFunction[Mobile, M],
                                                                                    keepLocked: M => Boolean,
                                                                                    override val universe: Universe)
  extends HasUniverse {
  def onTick(): Unit = {
    prioritizedTargets.invalidate()
    locked.filterNot(keepLocked).foreach { elem =>
      unlock_!(elem)
    }
  }

  def notifyLock_!(t: T, target: M): Unit = {
    locked += target
    val tar = Target(t, target)
    lockedTargets += tar
    assignments.put(target, tar)
  }

  private def unlock_!(target: M): Unit = {
    locked -= target
    val old = assignments.remove(target).get
    lockedTargets -= old
  }

  private val locked        = mutable.HashSet.empty[M]
  private val lockedTargets = mutable.HashSet.empty[Target[M]]
  private val assignments   = mutable.HashMap.empty[M, Target[M]]

  private val prioritizedTargets = LazyVal.from {
    val base = universe.enemyUnits.allByType[M]
               .collect(targetConstraint)

    spell.priorityRule.fold(base.toVector) { rule =>
      base.toVector.sortBy(rule)
    }
  }

  def suggestTargetFor(caster: T): Option[M] = {
    // for now, just pick the first in range that is not yet taken
    val range = spell.castRangeSquare

    prioritizedTargets.get.filterNot(locked)
    .find {_.currentPosition.distanceToSquared(caster.currentPosition) < range}
  }
}



