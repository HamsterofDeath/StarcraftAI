package pony.brain.modules

import pony.brain.{HasUniverse, Objective, SingleUnitBehaviour, Universe}
import pony.{CanCloak, CanUseStimpack, Ghost, InstantAttack, Medic, Mobile, MobileDetector, MobileRangeWeapon,
Orders, SCV, SupportUnit, Upgrades, Vulture, WrapsUnit}

import scala.collection.mutable

abstract class DefaultBehaviour[T <: Mobile : Manifest] extends HasUniverse {
  private val controlled           = new
      collection.mutable.HashMap[Objective, collection.mutable.Set[SingleUnitBehaviour[T]]]
      with mutable.MultiMap[Objective, SingleUnitBehaviour[T]]
  private val unit2behaviour       = mutable.HashMap.empty[T, SingleUnitBehaviour[T]]
  private val controlledUnits      = mutable.HashSet.empty[T]
  private var myUniverse: Universe = _
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
  def init_!(universe: Universe): Unit = {
    this.myUniverse = universe
  }
  override def universe = myUniverse
  def add_!(u: WrapsUnit, objective: Objective) = {
    assert(canControl(u))
    val behaviour = lift(u.asInstanceOf[T])
    controlled.addBinding(objective, behaviour)
    controlledUnits += behaviour.unit
    unit2behaviour.put(behaviour.unit, behaviour)
  }
  def cast = this.asInstanceOf[DefaultBehaviour[Mobile]]

  protected def lift(t: T): SingleUnitBehaviour[T]
}


object Terran {
  def allBehaviours(universe: Universe): Seq[DefaultBehaviour[Mobile]] = {
    val allOfThem = (
                      new StimSelf ::
                      /*
                                            new RepairDamagedBuilding ::
                                            new RepairDamagedUnit ::
                                           new RevealHiddenUnitsPermanenly ::
                                           new SetupMineField ::
                                           new Scout ::
                                           new Cloak ::
                                           new DoNotStray ::
                                           new StopMechanic ::
                                           new HealDamagedUnit ::
                                           new FixMedicalProblem ::
                                           new BlindDetector ::
                                           new Evade ::
                                           new StopToFire ::
                                           new FocusFire ::
                      */
                     Nil).map(_.cast)

    allOfThem.foreach(_.init_!(universe))
    allOfThem
  }

  class RepairDamagedBuilding extends DefaultBehaviour[SCV] {
    override protected def lift(t: SCV): SingleUnitBehaviour[SCV] = ???
  }
  class RepairDamagedUnit extends DefaultBehaviour[SCV] {
    override protected def lift(t: SCV): SingleUnitBehaviour[SCV] = ???
  }

  class StimSelf extends DefaultBehaviour[CanUseStimpack] {
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
  class RevealHiddenUnitsPermanenly extends DefaultBehaviour[MobileDetector] {
    override protected def lift(t: MobileDetector): SingleUnitBehaviour[MobileDetector] = ???
  }
  class SetupMineField extends DefaultBehaviour[Vulture] {
    override protected def lift(t: Vulture): SingleUnitBehaviour[Vulture] = ???
  }
  class Scout extends DefaultBehaviour[Mobile] {
    override protected def lift(t: Mobile): SingleUnitBehaviour[Mobile] = ???
  }
  class Cloak extends DefaultBehaviour[CanCloak] {
    override protected def lift(t: CanCloak): SingleUnitBehaviour[CanCloak] = ???
  }
  class DoNotStray extends DefaultBehaviour[SupportUnit] {
    override protected def lift(t: SupportUnit): SingleUnitBehaviour[SupportUnit] = ???
  }
  class StopMechanic extends DefaultBehaviour[Ghost] {
    override protected def lift(t: Ghost): SingleUnitBehaviour[Ghost] = ???
  }
  class HealDamagedUnit extends DefaultBehaviour[Medic] {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class FixMedicalProblem extends DefaultBehaviour[Medic] {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class BlindDetector extends DefaultBehaviour[Medic] {
    override protected def lift(t: Medic): SingleUnitBehaviour[Medic] = ???
  }
  class Evade extends DefaultBehaviour[Mobile] {
    override protected def lift(t: Mobile): SingleUnitBehaviour[Mobile] = ???
  }
  class StopToFire extends DefaultBehaviour[InstantAttack] {
    override protected def lift(t: InstantAttack): SingleUnitBehaviour[InstantAttack] = ???
  }
  class FocusFire extends DefaultBehaviour[MobileRangeWeapon] {
    override protected def lift(t: MobileRangeWeapon): SingleUnitBehaviour[MobileRangeWeapon] = ???
  }
}

