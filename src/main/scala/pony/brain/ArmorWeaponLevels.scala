package pony
package brain

import pony.units.WrapsUnit

import java.util

import bwapi.{Player, UnitType}

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
    val p          = unit.nativeUnit.getPlayer
    var byUnitType = cache.get(p)
    if (byUnitType == null) {
      byUnitType = new util.HashMap[UnitType, Levels]
      cache.put(p, byUnitType)
    }
    val unitType = unit.initialNativeType
    var armor    = byUnitType.get(unitType)
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
