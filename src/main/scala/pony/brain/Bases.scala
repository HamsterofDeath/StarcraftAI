package pony
package brain

import scala.collection.mutable.ArrayBuffer

class Bases(world: DefaultWorld, override val universe: Universe) extends HasUniverse {
  private val myBases          = ArrayBuffer.empty[Base]
  private val newBaseListeners = ArrayBuffer.empty[NewBaseListener]

  def isCovered(field: ResourceArea) = myBases.exists(b =>
    !b.mainBuilding.isFloating && b.mainBuilding.isInGame && b.resourceArea.contains(field)
  )

  def rich = {
    def singleValuable = myMineralFields.exists(_.value > 15000) &&
      myMineralFields.exists(_.patches.size >= 10)
    def multipleIncomes = myMineralFields.size >= 2 && myMineralFields.map(_.value).sum > 5000
    def muchGas         = myGeysirs.map(_.remaining).sum > 3000
    (singleValuable || multipleIncomes) && muchGas
  }

  def myMineralFields = myBases.flatMap(_.myMineralGroup).immutableView

  def myGeysirs = myBases.flatMap(_.myGeysirs).immutableView

  def richBasesCount = richBases.size

  def richBases = allBases.filter(_.resourceArea.exists(_.rich))

  def allBases = myBases.immutableView

  def finishedBases = allBases.filterNot(_.mainBuilding.isBeingCreated)

  def mainBase = myBases.headOption

  def tick(): Unit = {
    val all = ownUnits.allByType[MainBuilding]
    all.filterNot(known).foreach { main =>
      val newBase = new Base(main)
      myBases += newBase
      newBaseListeners.foreach(_.newBase(newBase))
    }

    myBases.retain(!_.mainBuilding.isDead)
  }

  def known(mb: MainBuilding) = myBases.exists(_.mainBuilding == mb)

  def register(newBaseListener: NewBaseListener, notifyForExisting: Boolean): Unit = {
    newBaseListeners += newBaseListener
    if (notifyForExisting) {
      myBases.foreach(newBaseListener.newBase)
    }
  }
}
