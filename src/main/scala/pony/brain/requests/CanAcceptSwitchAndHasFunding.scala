package pony
package brain
package requests

import pony.brain.jobs.JobHasFunding
import pony.units.WrapsUnit

trait CanAcceptSwitchAndHasFunding[T <: WrapsUnit]
    extends JobHasFunding[T] with CanAcceptUnitSwitch[T] {
  override def onStealUnit(): Unit = {
    super.onStealUnit()
  }
}
