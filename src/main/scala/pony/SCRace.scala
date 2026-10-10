package pony

import pony.tech.{TechTree, TerranTechTree}
import pony.units.{
  CommandCenter, DetectorBuilding, Drone, Dropship, Hive, MainBuilding, MissileTurret, Nexus, Overlord, PhotonCannon,
  Probe, Pylon, ResourceGatherPoint, SCV, Shuttle, SporeColony, SupplyDepot, SupplyProvider, TransporterUnit, WorkerUnit
}

import bwapi.Race

sealed trait SCRace {
  def isTerran  = false
  def isZerg    = false
  def isProtoss = false
  assert(isTerran ^ isZerg ^ isProtoss)

  def techTree: TechTree
  def specialize[T](unitType: Class[? <: T]) = {
    (if (classOf[WorkerUnit] >= unitType) {
       workerClass
     } else if (classOf[ResourceGatherPoint] >= unitType) {
       resourceDepositClass
     } else if (classOf[MainBuilding] >= unitType) {
       resourceDepositClass
     } else if (classOf[SupplyProvider] >= unitType) {
       supplyClass
     } else if (classOf[TransporterUnit] >= unitType) {
       transporterClass
     } else
       unitType).asInstanceOf[Class[T]]
  }
  def resourceDepositClass: Class[? <: MainBuilding]
  def workerClass: Class[? <: WorkerUnit]
  def transporterClass: Class[? <: TransporterUnit]
  def supplyClass: Class[? <: SupplyProvider]
  def detectorBuildingClass: Class[? <: DetectorBuilding]
}

object SCRace {
  def fromNative(r: Race) = {
    if (r == Race.Protoss) Protoss
    else if (r == Race.Terran) Terran
    else if (r == Race.Zerg) Zerg
    else {
      throw new IllegalStateException(s"Check this: $this")
    }
  }
}

case object Terran extends SCRace {

  override def isTerran = true

  override lazy val techTree = new TerranTechTree

  override def workerClass = classOf[SCV]

  override def transporterClass = classOf[Dropship]

  override def supplyClass = classOf[SupplyDepot]

  override def resourceDepositClass = classOf[CommandCenter]

  override def detectorBuildingClass = classOf[MissileTurret]
}

case object Zerg extends SCRace {

  override def isZerg = true

  override lazy val techTree = ???

  override def workerClass = classOf[Drone]

  override def transporterClass = classOf[Overlord]

  override def supplyClass = classOf[Overlord]

  override def resourceDepositClass = classOf[Hive]

  override def detectorBuildingClass = classOf[SporeColony]
}

case object Protoss extends SCRace {

  override def isProtoss = true

  override lazy val techTree = ???

  override def workerClass = classOf[Probe]

  override def transporterClass = classOf[Shuttle]

  override def supplyClass = classOf[Pylon]

  override def resourceDepositClass = classOf[Nexus]

  override def detectorBuildingClass = classOf[PhotonCannon]
}

case object Other extends SCRace {
  override val techTree = ???

  override def workerClass = ???

  override def transporterClass = ???

  override def supplyClass = ???

  override def resourceDepositClass = ???

  override def detectorBuildingClass = ???
}
