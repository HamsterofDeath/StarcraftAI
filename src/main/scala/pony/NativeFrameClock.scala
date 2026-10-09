package pony

private[pony] class NativeFrameClock {
  private var previous                   = -1
  def advance(nativeFrame: Int): Boolean = {
    if (nativeFrame <= previous) false
    else { previous = nativeFrame; true }
  }
}
