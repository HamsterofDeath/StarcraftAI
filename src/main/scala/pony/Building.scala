package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

trait Building extends BlockingTiles with CanDie with CanMorph {
  self =>
  override val armorType            = BuildingArmor
  private  val myFlying             = oncePerTick {
    nativeUnit.isFlying
  }
  private  val myAbandoned          = oncePerTick {
    isBeingCreated && incomplete && !isInstanceOf[Addon] && {
      val myClass = getClass
      val takenCareOf = unitManager.constructionsInProgress(myClass).exists { job =>
        job.building.contains(self)
      }
      !takenCareOf
    }

  }
  private  val myRemainingBuildTime = oncePerTick {
    nativeUnit.getRemainingBuildTime
  }

  def isFloating = myFlying.get

  def incomplete = currentNativeOrder == Order.IncompleteBuilding || remainingBuildTime > 0

  override def isHarmlessNow = super.isHarmlessNow || incomplete

  def remainingBuildTime = myRemainingBuildTime.get

  def isIncompleteAbandoned = myAbandoned.get
}
