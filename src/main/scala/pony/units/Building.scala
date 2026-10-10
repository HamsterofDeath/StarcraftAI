package pony
package units

import pony.combat.BuildingArmor

import bwapi._

trait Building extends BlockingTiles with CanDie with CanMorph {
  self =>
  override val armorType = BuildingArmor
  private val myFlying   = oncePerTick {
    nativeUnit.isFlying
  }
  private val myAbandoned = oncePerTick {
    isBeingCreated && incomplete && !isInstanceOf[Addon] && {
      val myClass     = getClass
      val takenCareOf = unitManager.constructionsInProgress(myClass).exists { job =>
        job.building.contains(self)
      }
      !takenCareOf
    }

  }
  private val myRemainingBuildTime = oncePerTick {
    nativeUnit.getRemainingBuildTime
  }

  def isFloating = myFlying.get

  def incomplete = currentNativeOrder == Order.IncompleteBuilding || remainingBuildTime > 0

  override def isHarmlessNow = super.isHarmlessNow || incomplete

  def remainingBuildTime = myRemainingBuildTime.get

  def isIncompleteAbandoned = myAbandoned.get
}
