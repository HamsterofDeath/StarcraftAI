package pony
package brain

import java.util

import bwapi.{Player, TechType, UnitType, UpgradeType}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait OnResearchComplete {
  def onComplete(upgrade: Upgrade): Unit
}

class ArmorWeaponLevels(override val universe: Universe) extends HasUniverse {
  private val cache = new java.util.HashMap[Player, java.util.HashMap[UnitType, Levels]]

  universe.register_!(() => {
    ifNth(Primes.prime23) {
      cache.clear()
    }
  })

  def currentWeaponLevelOf(weaponOwner: WrapsUnit) = {
    getUpgradesOf(weaponOwner).weapon
  }

  def currentArmorLevelOf(unit: WrapsUnit) = {
    getUpgradesOf(unit).armor
  }

  private def getUpgradesOf(unit: WrapsUnit) = {
    val p = unit.nativeUnit.getPlayer
    var byUnitType = cache.get(p)
    if (byUnitType == null) {
      byUnitType = new util.HashMap[UnitType, Levels]
      cache.put(p, byUnitType)
    }
    val unitType = unit.initialNativeType
    var armor = byUnitType.get(unitType)
    if (armor == null) {
      val gLevel = p.getUpgradeLevel(unitType.groundWeapon().upgradeType())
      val aLevel = p.getUpgradeLevel(unitType.airWeapon().upgradeType())
      armor = Levels(p.armor(unitType), gLevel max aLevel)
      byUnitType.put(unitType, armor)
    }
    armor
  }

  case class Levels(armor: Int, weapon: Int)
}

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
    onResearchCompleteListener.foreach {_.onComplete(upgrade)}

    upgrade.nativeType.fold(
      u => {
        val actual = universe.world.nativeGame.self().getUpgradeLevel(u)
        val expected = upgradeLevelOf(u)
        assert(actual == expected,
          s"Expected level $expected for $upgrade, but game said it was $actual")
      },
      t => assert(isTechResearchInNativeGame(t), s"Out of sync! $upgrade"))

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