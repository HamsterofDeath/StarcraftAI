package pony

object OnKillListener {
  def on[T <: WrapsUnit, X](unit: T, doThis: () => X) = new OnKillListener[T](unit) {
    override def onKill(t: T): Unit = {
      assert(t == this.unit)
      doThis()
    }
  }
}

abstract class OnKillListener[T <: WrapsUnit](val unit: T) {
  def onKill(t: T): Unit

  def onKillUnTyped(t: WrapsUnit) = {
    assert(unit == t)
    onKill(unit)
  }

  def nativeUnitId = unit.nativeUnitId
}
