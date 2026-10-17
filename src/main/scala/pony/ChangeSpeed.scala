package pony

class ChangeSpeed extends AIPluginRunOnce {
  override def runOnce(): Unit = {
    val configured = sys.props.get("twailight.localSpeed").flatMap(_.toIntOption).getOrElse(0)
    if (configured > 0) debugger.speed(configured)
    else debugger.fastest()
  }
}
