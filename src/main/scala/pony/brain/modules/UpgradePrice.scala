package pony
package brain
package modules

trait UpgradePrice {

  def forUpgrade: Upgrade

  def nextMineralPrice: Int

  def nextGasPrice: Int

}
