package pony
package units

trait CanBurrow extends ZergMobileUnit with VirtualPosition with VirtualHitPoints with CanHide {

  private var virtualBurrowed = false

  override def isVisible = !virtualBurrowed

  private val myBurrowed = oncePerTick {
    nativeUnit.isBurrowed
  }

  def isBurrowed = myBurrowed.get

  override def isDead = {
    super.isDead && currentTile.isOutsideOfGame
  }

  override def onTick_!() = {
    super.onTick_!()
    // units that can burrow somehow lose all their attributes and even stop officially existing,
    // but pop up later as soon as they unburrow. the ai needs to keep track of them
    // if they start burrowed, they might start with officially 0 hp...
    if (exists && isBurrowed && currentTick < 2) {
      remember_!()
      virtualBurrowed = true
    }
    if (currentNativeOrder == bwapi.Order.Burrowing || isBurrowed) {
      remember_!()
      virtualBurrowed = true
    } else if (currentNativeOrder == bwapi.Order.Unburrowing) {
      forget_!()
      virtualBurrowed = false
    }
  }
}
