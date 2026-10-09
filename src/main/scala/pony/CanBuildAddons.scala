package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait CanBuildAddons extends Building {
  private val myAddonArea = oncePerTick {
    Area(area.lowerRight.movedBy(1, -1), Size(2, 2))
  }
  private var attached    = Option.empty[Addon]
  def positionedNextTo(addon: Addon) = {
    myAddonArea.upperLeft == addon.tilePosition
  }
  def addonArea = myAddonArea.get
  def canBuildAddon(addon: Class[? <: Addon]) = race.techTree.canBuildAddon(getClass, addon)
  def isBuildingAddon = Option(nativeUnit.getAddon).exists(_.getRemainingBuildTime > 0)

  def hasCompleteAddon = Option(nativeUnit.getAddon).exists(_.getRemainingBuildTime == 0)

  def notifyAttach_!(addon: Addon): Unit = {
    trace(s"Addon $addon attached to $this")
    assert(attached.isEmpty)
    attached = Some(addon)
  }
  def hasAddonAttached = attached.isDefined
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
