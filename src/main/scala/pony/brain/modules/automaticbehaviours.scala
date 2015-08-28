package pony.brain.modules

import pony.brain.{HasUniverse, Universe}
import pony.{CanCloak, CanDetectHidden, CanUseStimpack, Comsat, Ghost, InstantAttack, MapTilePosition, Medic, Mobile,
Orders, RangeWeapon, SCV, SupportUnit, UnitOrder, Upgrades, Vulture, WrapsUnit}

import scala.collection.mutable

trait SingleUnitBehaviour[T <: WrapsUnit] {
  def toOrder(what: Objective): Seq[UnitOrder]
  def preconditionOk = true
}

abstract class DefaultBehaviour[T <: WrapsUnit : Manifest] extends HasUniverse {
  private val controlled           = new
      collection.mutable.HashMap[Objective, collection.mutable.Set[SingleUnitBehaviour[T]]]
      with mutable.MultiMap[Objective, SingleUnitBehaviour[T]]
  private var myUniverse: Universe = _
  def init_!(universe: Universe): Unit = {
    this.myUniverse = universe
  }
  override def universe = myUniverse
  def add_!(u: WrapsUnit, objective: Objective) = {
    assert(canControl(u))
    controlled.addBinding(objective, lift(u.asInstanceOf[T]))
  }
  def canControl(u: WrapsUnit) = {
    manifest[T].runtimeClass.isAssignableFrom(u.getClass)
  }
  def commands = controlled.flatMap { case (what, who) =>
    who.flatMap(_.toOrder(what))
  }.toVector
  def cast = this.asInstanceOf[DefaultBehaviour[WrapsUnit]]
  protected def lift(t: T): SingleUnitBehaviour[T]
}

sealed trait Behaviour
case object AggressiveMove extends Behaviour
case object HoldPosition extends Behaviour
case object FallBack extends Behaviour

case class Objective(position: MapTilePosition, radius: Int, how: Behaviour)

object Terran {
  def all(universe: Universe): Seq[DefaultBehaviour[WrapsUnit]] = {
    val allOfThem = (new RepairDamagedBuilding ::
                     new RepairDamagedUnit ::
                     new StimSelf ::
                     new RevealHiddenUnits ::
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
    override protected def lift(t: CanUseStimpack) = new SingleUnitBehaviour[CanUseStimpack] {

      override def preconditionOk = upgrades.hasResearched(Upgrades.Terran.InfantryCooldown)

      override def toOrder(what: Objective) = {
        if (t.isAttacking && !t.isStimmed) {
          List(Orders.Stimpack(t))
        } else {
          Nil
        }
      }
    }
  }
  class RevealHiddenUnits extends DefaultBehaviour[Comsat] {
    override protected def lift(t: Comsat): SingleUnitBehaviour[Comsat] = ???
  }
  class RevealHiddenUnitsPermanenly extends DefaultBehaviour[CanDetectHidden] {
    override protected def lift(t: CanDetectHidden): SingleUnitBehaviour[CanDetectHidden] = ???
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
  class FocusFire extends DefaultBehaviour[RangeWeapon] {
    override protected def lift(t: RangeWeapon): SingleUnitBehaviour[RangeWeapon] = ???
  }
}

