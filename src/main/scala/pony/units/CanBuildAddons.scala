package pony
package units

import pony.geometry.{Area, Size}

trait CanBuildAddons extends Building {
  private val myAddonArea = oncePerTick {
    Area(area.lowerRight.movedBy(1, -1), Size(2, 2))
  }
  private var attached               = Option.empty[Addon]
  def positionedNextTo(addon: Addon) = {
    myAddonArea.upperLeft == addon.tilePosition
  }
  def addonArea                               = myAddonArea.get
  def canBuildAddon(addon: Class[? <: Addon]) = race.techTree.canBuildAddon(getClass, addon)
  def isBuildingAddon                         = Option(nativeUnit.getAddon).exists(_.getRemainingBuildTime > 0)

  def hasCompleteAddon = Option(nativeUnit.getAddon).exists(_.getRemainingBuildTime == 0)

  private var addonRefusals    = 0
  private var lastAddonRefusal = 0

  /** An add-on job ended without the add-on ever starting, usually because its spot is blocked. */
  def addonRefused_!(): Unit = { addonRefusals += 1; lastAddonRefusal = currentTick }
  def addonStarted_!(): Unit = addonRefusals = 0

  /**
    * Offered for an add-on job. After three refused jobs in a row the building rests for two minutes: in game 2 vs Zerg
    * on a82be0c a command center next to a cliff took a new Comsat job every few seconds for 13 minutes, and the depot
    * relocation, which waits for an idle depot, never got it.
    */
  def mayTakeAddon = addonRefusals < 3 || currentTick - lastAddonRefusal > 24 * 120

  def notifyAttach_!(addon: Addon): Unit = {
    trace(s"Addon $addon attached to $this")
    assert(attached.isEmpty)
    attached = Some(addon)
  }
  def hasAddonAttached          = attached.isDefined
  override def onTick_!(): Unit = {
    super.onTick_!()
    attached.filter(_.isDead).foreach { dead =>
      notifyDetach_!(dead)
    }
  }
  def notifyDetach_!(addon: Addon): Unit = {
    trace(s"Addon $addon detached from $this")
    assert(attached.isDefined)
    assert(attached.contains(addon))
    attached = None
  }
}
