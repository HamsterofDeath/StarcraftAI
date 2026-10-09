package pony

object SynchronizedLazyVal {
  def from[T](t: => T) = new SynchronizedLazyVal(t)
}

class SynchronizedLazyVal[T](gen: => T, onValueChange: Option[() => Unit] = None)
    extends LazyVal(gen, onValueChange) {

  private var lastGeneratedValue = Option.empty[T]

  override protected def allowMultiRead = true

  override def invalidate() = synchronized {
    super.invalidate()
  }

  override def get = synchronized {
    if (isOnCreationThread) {
      val ret = super.get
      lastGeneratedValue = ret.toSome
      ret
    } else {
      lastGeneratedValue.getOr("Data generation is limited to creation thread")
    }
  }
}
