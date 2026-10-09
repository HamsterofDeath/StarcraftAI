package pony

trait AIPluginRunOnce extends AIPlugIn {
  private var executed = false
  def runOnce(): Unit
  override protected def tickPlugIn(): Unit = {
    if (!executed) {
      executed = true
      runOnce()
    }
  }
}
