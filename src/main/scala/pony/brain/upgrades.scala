package pony
package brain

import bwapi.TechType

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait OnResearchComplete {
  def onComplete(upgrade: Upgrade): Unit
}

class UpgradeManager(override val universe: Universe) extends HasUniverse {
  def armorForUnitType(unitType: WrapsUnit) = {
    val relevantUpgrade = unitType match {
      case _: Building => Upgrades.Fake.BuildingArmor
      case _: Infantry =>
        unitType.race match {
          case Terran => Upgrades.Terran.InfantryArmor
          case Protoss => Upgrades.Protoss.InfantryArmor
          case Zerg => Upgrades.Zerg.InfantryArmor
        }
      case _: Vehicle =>
        unitType.race match {
          case Terran => Upgrades.Terran.VehicleArmor
          case Protoss => Upgrades.Protoss.VehicleArmor
          case Zerg => Upgrades.Zerg.VehicleArmor
        }
      case _: Ship =>
        unitType.race match {
          case Terran => Upgrades.Terran.ShipArmor
          case Protoss => Upgrades.Protoss.ShipArmor
          case Zerg => Upgrades.Zerg.ShipArmor
        }
    }
    researched.getOrElse(relevantUpgrade, 0)
  }

  private val onResearchCompleteListener = ArrayBuffer.empty[OnResearchComplete]

  private val researched = mutable.HashMap.empty[Upgrade, Int]
  researched += ((Upgrades.Fake.BuildingArmor, 1))

  Upgrades.allTech.filter(t => isTechResearchInNativeGame(t.nativeTech)).foreach { up =>
    researched.put(up, 1)
  }

  private def isTechResearchInNativeGame(t: TechType) = {
    universe.world.nativeGame.self().hasResearched(t)
  }

  def notifyResearched_!(upgrade: Upgrade): Unit = {
    upgrade.nativeType.fold(
      u => {/* how to check this? */},
      t => assert(isTechResearchInNativeGame(t), s"Out of sync! $upgrade"))

    researched += ((upgrade, researched.getOrElse(upgrade, 0) + 1))
    onResearchCompleteListener.foreach {_.onComplete(upgrade)}
  }

  def hasResearched(upgrade: Upgrade) = researched.contains(upgrade)

  def register_!(listener: OnResearchComplete): Unit = {
    onResearchCompleteListener += listener
  }

}