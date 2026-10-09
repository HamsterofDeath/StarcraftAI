package pony
package brain
package modules
package strategy

final case class UpgradeToResearch(upgrade: Upgrade)(active: => Boolean) {
  val maxLevel = upgrade.nativeType.fold(_.maxRepeats, _ => 1)

  def isActive = active
}
