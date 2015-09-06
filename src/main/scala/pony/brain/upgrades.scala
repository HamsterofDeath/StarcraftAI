package pony
package brain

import bwapi.TechType

import scala.collection.mutable

class UpgradeManager(override val universe: Universe) extends HasUniverse {
  private val researched = mutable.HashMap.empty[Upgrade, Int]

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
  }

  def hasResearched(upgrade: Upgrade) = researched.contains(upgrade)

}