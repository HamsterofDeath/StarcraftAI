package pony

class ChangeSpeed extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    debugger.fastest()
  }
}
