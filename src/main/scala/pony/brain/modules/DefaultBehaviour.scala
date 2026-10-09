package pony
package brain
package modules

import scala.collection.mutable
import scala.reflect.ClassTag

abstract class DefaultBehaviour[T <: WrapsUnit: ClassTag](override val universe: Universe)
    extends HasUniverse {

  private val asEmployer      = new Employer[T](universe)
  private val unit2behaviour  = mutable.HashMap.empty[T, SingleUnitBehaviour[T]]
  private val controlledUnits = mutable.HashSet.empty[T]

  def employer = asEmployer

  override def onTick_!(): Unit = {
    super.onTick_!()
    val remove = controlledUnits.filterNot(_.isInGame)
    controlledUnits --= remove
    unit2behaviour --= remove
  }

  def renderDebug_!(renderer: Renderer): Unit = {}

  def behaviourOf(unit: T) = {
    ifControlsOpt(unit) { identity }
  }

  def ifControlsOpt[R](m: T)(f: (SingleUnitBehaviour[T]) => R) = {
    ifControls(m, Option.empty[R])(e => Some(f(e)))
  }

  def ifControls[R](m: T, ifNot: R)(f: (SingleUnitBehaviour[T]) => R) = {
    if (controls(m)) {
      f(unit2behaviour(assumeSafe(m)))
    } else {
      ifNot
    }
  }

  def controls(unit: T) = {
    canControl(unit) && controlledUnits.contains(assumeSafe(unit))
  }

  def canControl(u: WrapsUnit) = {
    implicitly[ClassTag[T]].runtimeClass.isInstance(u) && !u.isInstanceOf[AutoPilot]
  }

  def assumeSafe(unit: WrapsUnit): T = unit.asInstanceOf[T]

  def add_!(u: WrapsUnit, objective: Objective) = {
    assert(canControl(u))
    val behaviour = wrapBase(u.asInstanceOf[T])
    controlledUnits += behaviour.unit
    unit2behaviour.put(behaviour.unit, behaviour)
  }

  def cast = this.asInstanceOf[DefaultBehaviour[WrapsUnit]]

  protected def meta = SingleUnitBehaviourMeta(
    priority,
    refuseCommandsForTicks,
    forceRepeatedCommands
  )

  def forceRepeatedCommands = false

  def priority = SecondPriority.Default

  def refuseCommandsForTicks = 0

  protected def wrapBase(unit: T): SingleUnitBehaviour[T]

}
