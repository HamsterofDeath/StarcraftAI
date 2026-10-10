package pony
package brain

import pony.brain.jobs.JobOrSubJob
import pony.units.WrapsUnit

abstract class SingleUnitBehaviour[+T <: WrapsUnit](val unit: T, meta: SingleUnitBehaviourMeta)
    extends JobOrSubJob[T] {
  def isNoopTask = false

  private var skipFor = 0

  override def universe = unit.universe

  def onStealUnit(): Unit = {}

  protected def butOnlyIf = true

  def orderForTick(what: Objective) = {
    if (skipFor > 0) {
      skipFor -= 1
      Nil
    } else {
      val interrupt = higherPriorityOrder
      if (interrupt.isEmpty) {
        if (butOnlyIf) {
          toOrder(what)
        } else {
          Nil
        }
      } else {
        interrupt
      }
    }
  }

  def preconditionOk = true

  def describeShort: String

  def priority = meta.priority

  def blocksForTicks = meta.refuseCommandsForTicks

  def forceRepeats = meta.forceRepeats

  def canInterrupt = true

  def skipFor(i: Int): Unit = {
    skipFor = i
  }

  protected def toOrder(what: Objective): Seq[UnitOrder]
}
