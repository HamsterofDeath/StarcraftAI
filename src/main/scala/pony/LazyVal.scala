package pony

class LazyVal[T](gen: => T, onValueChange: Option[() => Unit] = None) extends Serializable {
  protected def allowMultiRead = false
  private   val creationThread = Thread.currentThread()
  private   var locked         = false
  protected var evaluated      = false
  protected var value: T       = _

  def lockValueForever(): Unit = {
    locked = true
  }

  def invalidate(): Unit = {
    if (!locked) {
      evaluated = false
      if (onValueChange.isEmpty) {
        value = null.asInstanceOf[T]
      }
    }
  }

  override def toString = s"LazyVal($get)"

  def get = {
    assert(isOnCreationThread)
    if (!evaluated)
      if (onValueChange.isDefined) {
        val newVal = gen
        if (newVal != value) {
          onValueChange.foreach(_ ())
          value = newVal
        }
      } else {
        value = gen
        assert(value != null)
      }

    evaluated = true
    assert(value != null)
    value
  }

  protected def isOnCreationThread: Boolean = {
    Thread.currentThread() == creationThread
  }
}

object LazyVal {
  def from[T](t: => T) = new LazyVal(t, None)

  def from[T](t: => T, onValueChange: => Unit) = new LazyVal(t, Some(() => onValueChange))
}
