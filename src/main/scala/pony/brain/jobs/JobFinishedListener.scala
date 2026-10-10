package pony
package brain
package jobs

import pony.units.WrapsUnit

trait JobFinishedListener[T <: WrapsUnit] {
  def onFinishOrFail(failed: Boolean): Unit
}
