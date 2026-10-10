package pony
package units

import pony.tech.Upgrade

import bwapi._

trait Upgrader extends Controllable with Building {
  def canUpgrade(upgrade: Upgrade) = race.techTree.canUpgrade(getClass, upgrade)
  def isDoingResearch              = currentOrder == Order.ResearchTech || currentOrder == Order.Upgrade
}
