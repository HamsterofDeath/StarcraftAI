package pony
package brain

trait IsTicked {

  private var lastCalled = 0

  def onTick_!(): Unit = {
    lastCalled = currentTick
  }

  def assertCalled() = {
    if (lastCalled != currentTick) {
      onTick_!()
    }
  }

  protected def currentTick: Int

}
