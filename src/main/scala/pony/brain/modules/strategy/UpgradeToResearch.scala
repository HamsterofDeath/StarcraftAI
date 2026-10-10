package pony
package brain
package modules
package strategy

import pony.tech.Upgrade

final case class UpgradeToResearch(upgrade: Upgrade)(active: => Boolean) {
  val maxLevel = upgrade.nativeType.fold(_.maxRepeats, _ => 1)

  def isActive = active
}
