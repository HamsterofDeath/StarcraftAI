package pony
package brain

import scala.collection.mutable

class UpgradeManager(override val universe: Universe) extends HasUniverse {
  private val researched = mutable.HashMap.empty[Upgrade, Int]

  def notifyResearched_!(upgrade: Upgrade): Unit = {
    upgrade.nativeType.fold(
      u => {/* how to check this? */},
      t => assert(universe.world.nativeGame.self().hasResearched(t), s"Out of sync! $upgrade"))

    researched += ((upgrade, researched.getOrElse(upgrade, 0) + 1))
  }

  def hasResearched(upgrade: Upgrade) = researched.contains(upgrade)

}