package pony
package brain
package budget

trait HasFunding {
  private var explicitlyUnlocked         = false
  private var autoUnlocked               = false
  private var unlockedDebug: Any         = null
  private var unlockedManuallyDebug: Any = null
  private var doNotUnlock                = false

  def stopManagingResource_!(): Unit = {
    trace(s"$this is no longer taking car of $proofForFunding")
    doNotUnlock = true
  }
  def proofForFunding: ResourceApproval
  def resources: ResourceManager
  def stillLocksResources                  = !explicitlyUnlocked && !autoUnlocked
  def notifyResourcesDisapproved_!(): Unit = {
    trace(s"Resources of $this just got disapproved")
    unlockManually_!()
  }

  def unlockManually_!(): Unit = {
    if (doNotUnlock) {
      trace(s"Supposed to unlock resources $proofForFunding of $this, but marked as noop")
    } else {
      assert(!explicitlyUnlocked, s"Don't do that twice")
      assert(!autoUnlocked, s"Too slow")
      trace(s"Manually unlocking $proofForFunding of $this")
      unlock_!()
      explicitlyUnlocked = true
      unlockedManuallyDebug = Thread.currentThread().getStackTrace
    }
  }

  def unlock_!(): Unit = {
    if (!doNotUnlock) {
      if (!explicitlyUnlocked) {
        assert(!autoUnlocked, s"Already unlocked: $this")
        proofForFunding match {
          case suc: ResourceApprovalSuccess =>
            autoUnlocked = true
            trace(s"Unlocking funds of $this : $proofForFunding")
            resources.unlock_!(suc)
            unlockedDebug = Thread.currentThread().getStackTrace
          case _ =>
        }
      } else {
        trace(s"Not unlocking $proofForFunding of $this automatically, someone already did that")
      }
    } else {
      trace(s"Tried to unlock resources $proofForFunding of $this, but marked as noop")
    }
  }
}
