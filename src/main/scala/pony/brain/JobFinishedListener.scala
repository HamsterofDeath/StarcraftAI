package pony
package brain

trait JobFinishedListener[T <: WrapsUnit] {
  def onFinishOrFail(failed: Boolean): Unit
}
