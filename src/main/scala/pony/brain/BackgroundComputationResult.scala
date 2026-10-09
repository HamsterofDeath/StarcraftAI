package pony
package brain

trait BackgroundComputationResult[T <: WrapsUnit] {

  def jobs: Iterable[UnitWithJob[T]]

  def orders = jobs.flatMap(_.ordersForThisTick)
  def afterComputation(): Unit
  def repeatOrderIssue: Boolean
}

object BackgroundComputationResult {
  def nothing[T <: WrapsUnit](cleanUp: () => Unit): BackgroundComputationResult[T] = new
      BackgroundComputationResult[T] {
    override def repeatOrderIssue: Boolean = false

    override def afterComputation(): Unit = cleanUp()

    override def jobs: Iterable[UnitWithJob[T]] = Nil
  }

  def result[T <: WrapsUnit, J <: UnitWithJob[T]](myJobs: Iterable[() => J],
                                                  checkValidityNow: () => Boolean,
                                                  canCreateNow: () => Boolean = () => true)
                                                 (afterComputationDone: Iterable[J] => Unit) = new
      BackgroundComputationResult[T] {

    private lazy val executed = if (canCreateNow()) myJobs.map(_.apply()) else Nil

    override def jobs = executed

    override def repeatOrderIssue = checkValidityNow()

    override def afterComputation() = afterComputationDone(executed)

    override def orders = jobs.flatMap(_.ordersForThisTick)
  }
}
