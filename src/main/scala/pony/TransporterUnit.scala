package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait TransporterUnit extends AirUnit {
  override def isNonFighter = true
  private val myPickingUp = oncePerTick {
    nativeUnit.getOrderTarget != null
  }
  private val myLoaded    = oncePerTick {
    nativeUnit.getLoadedUnits.asScala.flatMap { u =>
      ownUnits.byNative(u).asInstanceOf[Option[GroundUnit]]
    }.toSet
  }

  def nearestDropTile = {
    ferryManager.nearestDropPointTo(currentTile)
  }

  def isPickingUp = myPickingUp.get
  def loaded = myLoaded.get
  def isCarrying(gu: GroundUnit) = myLoaded(gu)
  def canDropHere = ferryManager.canDropHere(currentTile)

  def hasUnitsLoaded = myLoaded.nonEmpty
}
