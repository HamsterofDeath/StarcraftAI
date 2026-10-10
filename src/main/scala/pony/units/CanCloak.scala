package pony
package units

trait CanCloak extends Mobile with CanHide {

  override def isAttackable = super.isAttackable && isExposed

  override def isExposed = super.isExposed && isDecloaked

  private val cloaked = oncePerTick {
    nativeUnit.isCloaked
  }

  private val decloaked = oncePerTick {
    nativeUnit.isDetected
  }

  override def shortDebugString = {
    val c = if (isCloaked) "c" else ""
    val d = if (isDecloaked) "d" else ""
    val e = if (isExposed) "e" else ""
    val v = if (isVisible) "v" else ""
    s"${super.shortDebugString}.$c$d$e"
  }

  def isCloaked = cloaked.get

  def isDecloaked = decloaked.get

  override def isVisible = !isCloaked || isDecloaked

}
