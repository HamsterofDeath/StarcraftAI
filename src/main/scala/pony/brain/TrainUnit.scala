package pony
package brain

class TrainUnit[F <: UnitFactory, T <: Mobile](
    factory: F,
    trainType: Class[? <: T],
    employer: Employer[F],
    funding: ResourceApprovalSuccess
) extends UnitWithJob[F](employer, factory, Priority.Default) with JobHasFunding[F] with IssueOrderNTimes[F]
    with CreatesUnit[F] {

  private val patience = 20

  private var startedToProduce = false

  def requestedType = trainType

  override def proofForFunding = funding

  override def getOrder: Seq[UnitOrder] = {
    assert(!failedOrObsolete, s"$this has failed, but is still asked for its order")
    info(s"Training $trainType")
    Orders.Train(unit, trainType).toSeq
  }

  override def onTick_!(): Unit = {
    super.onTick_!()

    if (!startedToProduce && ageSinceLastReset > patience && factory.isProducing) {
      startedToProduce = true
      trace(s"Job $this started to produce ${trainType.className}")
      if (classOf[WorkerUnit] >= trainType)
        NativeMatchEvidence.trace("scv-production", s"depot=${factory.nativeUnitId} tile=${factory.tilePosition}")
      unlockManually_!()
    }
  }

  override def jobHasFailedWithoutDeath: Boolean = {
    ageSinceLastReset > patience + 5 && !startedToProduce
  }

  override def isFinished = {
    val idle   = !factory.isProducing
    val isLazy = !factory.nativeUnit.isTraining
    val ret    = idle && isLazy && startedToProduce
    ret
  }

  override def shortDebugString: String = s"Train ${trainType.className}"
}
