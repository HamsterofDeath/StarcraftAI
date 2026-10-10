package pony
package brain

import pony.tech.{Upgrade, Upgrades}
import pony.units.WrapsUnit

import bwapi.{TechType, UpgradeType}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class UpgradeManager(override val universe: Universe) extends HasUniverse {
  private val armorLevels                = new ArmorWeaponLevels(universe)
  private val onResearchCompleteListener = ArrayBuffer.empty[OnResearchComplete]
  private val researched                 = mutable.HashMap.empty[Upgrade, Int]

  def armorForUnitType(unit: WrapsUnit) = {
    armorLevels.currentArmorLevelOf(unit)
  }

  def weaponLevelOf(weaponOwner: WrapsUnit) = {
    armorLevels.currentWeaponLevelOf(weaponOwner)
  }
  researched += ((Upgrades.Fake.BuildingArmor, 1))

  Upgrades.allTech.filter(t => isTechResearchInNativeGame(t.nativeTech)).foreach { up =>
    researched.put(up, 1)
  }

  def notifyResearched_!(upgrade: Upgrade): Unit = {
    researched += ((upgrade, researched.getOrElse(upgrade, 0) + 1))
    onResearchCompleteListener.foreach { _.onComplete(upgrade) }

    upgrade.nativeType.fold(
      u => {
        val actual   = universe.world.nativeGame.self().getUpgradeLevel(u)
        val expected = upgradeLevelOf(u)
        if (actual != expected) {
          warn(s"Upgrade level mismatch for $upgrade: expected $expected but game said $actual")
          researched.put(upgrade, actual)
        }
      },
      t => warn(s"Out of sync! $upgrade", !isTechResearchInNativeGame(t))
    )

  }

  def isTechResearchInNativeGame(t: TechType) = {
    universe.world.nativeGame.self().hasResearched(t)
  }

  def upgradeLevelOf(u: UpgradeType) = {
    researched.getOrElse(new Upgrade(u), 0)
  }

  def hasResearched(upgrade: Upgrade) = researched.contains(upgrade)

  def register_!(listener: OnResearchComplete): Unit = {
    onResearchCompleteListener += listener
  }

}
