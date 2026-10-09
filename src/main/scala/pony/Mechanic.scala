package pony

trait Mechanic extends Mobile {

  private val myLocked = oncePerTick {
    nativeUnit.getLockdownTimer > 0
  }

  def isLocked = myLocked.get
}
