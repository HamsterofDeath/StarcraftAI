package pony
package brain

class ConstructAddon[W <: CanBuildAddons, A <: Addon](employer: Employer[W],
                                                      basis: W,
                                                      what: Class[_ <: A],
                                                      funding: ResourceApproval)
  extends UnitWithJob[W](employer, basis, Priority.Addon) with JobHasFunding[W] with CreatesUnit[W] with IssueOrderNTimes[W] {
  assert(!basis.isBuildingAddon)
  assert(!basis.hasCompleteAddon)
  assert(!basis.hasAddonAttached)

  private var startedConstruction = false
  private var stoppedConstruction = false

  override def times: Int = 10

  override def shortDebugString: String = s"Construct ${builtWhat.className}"

  private def builtWhat = what

  override def isFinished: Boolean = {
    startedConstruction && stoppedConstruction
  }

  override def onTick_!(): Unit = {
    super.onTick_!()
    assert(failedOrObsolete || resources.hasStillLocked(proofForFunding),
      s"Someone stole $proofForFunding from $this")
    if (!startedConstruction) {
      startedConstruction = basis.isBuildingAddon
    }
    if (startedConstruction && !stoppedConstruction) {
      val addon = basis.hasCompleteAddon
      stoppedConstruction = addon
    }
  }

  override def proofForFunding = funding

  override def onFinishOrFail(): Unit = {
    super.onFinishOrFail()
    universe.ownUnits.allAddons.find(e => basis.positionedNextTo(e)).foreach { e =>
      basis.notifyAttach_!(e)
    }
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    def myFail = ageSinceLastReset > 50 && !startedConstruction
    super.jobHasFailedWithoutDeath || myFail
  }

  override def getOrder = Orders.ConstructAddon(basis, builtWhat).toSeq
}
