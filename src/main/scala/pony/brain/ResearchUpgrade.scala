package pony
package brain

class ResearchUpgrade[U <: Upgrader](
    employer: Employer[U],
    basis: U,
    what: Upgrade,
    funding: ResourceApproval
) extends UnitWithJob[U](employer, basis, Priority.Upgrades) with JobHasFunding[U] with IssueOrderNTimes[U] {

  private var startedResearch     = false
  private var stoppedResearch     = false
  private var stoppedAtTick       = Option.empty[Int]
  private var gameConfirmsUpgrade = false

  override def shortDebugString: String = s"Research $what"

  override def isFinished: Boolean = {
    startedResearch && stoppedResearch && gameConfirmsUpgrade
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    super.jobHasFailedWithoutDeath || (ageSinceLastReset > 10 && !startedResearch) || {
      stoppedResearch && !gameConfirmsUpgrade && currentTick - stoppedAtTick.get > 24
    }
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    if (!startedResearch && basis.isDoingResearch) {
      startedResearch = true
    }
    if (startedResearch && !basis.isDoingResearch) {
      stoppedResearch = true
      stoppedAtTick = currentTick.toSome
    }
    if (stoppedResearch && !gameConfirmsUpgrade) {
      if (isResearchedInRealGame) {
        gameConfirmsUpgrade = true
      }
    }
  }

  private def isResearchedInRealGame = {
    what.nativeType.fold(
      u => {
        val actual   = universe.world.nativeGame.self().getUpgradeLevel(u)
        val expected = universe.upgrades.upgradeLevelOf(u)
        actual == expected + 1
      },
      t => universe.upgrades.isTechResearchInNativeGame(t)
    )

  }

  override def getOrder =
    Orders.Research(basis, what).toSeq

  override def proofForFunding = funding

}
