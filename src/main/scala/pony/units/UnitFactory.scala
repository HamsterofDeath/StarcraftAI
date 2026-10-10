package pony
package units

import bwapi._

trait UnitFactory extends Building with Controllable {
  def canBuild[T <: Mobile](typeOfUnit: Class[? <: T]) = {
    race.techTree.canBuild(getClass, typeOfUnit)
  }

  def isProducing = nativeUnit.getOrder == Order.Train || nativeUnit.getRemainingTrainTime > 0
}
