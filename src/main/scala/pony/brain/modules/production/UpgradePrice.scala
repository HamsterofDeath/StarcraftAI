package pony
package brain
package modules
package production

import pony.tech.Upgrade

trait UpgradePrice {

  def forUpgrade: Upgrade

  def nextMineralPrice: Int

  def nextGasPrice: Int

}
