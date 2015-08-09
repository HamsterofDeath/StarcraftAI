package pony
package brain
package modules

class ProvideFactories(universe: Universe) extends OrderlessAIModule[WorkerUnit](universe) {

  override def onTick(): Unit = {
    // TODO adjust number of factories to money income
    unitManager.
  }
  private def evaluateCapacities = {
    // TODO actually calculate this
    IdealProducerCount(classOf[])
  }
  case class IdealProducerCount[T <: UnitFactory](typeOfFactory: Class[_ <: UnitFactory], ideal: Int)
}

class ProvideArmy(universe: Universe) extends OrderlessAIModule[UnitFactory](universe) {
  override def onTick(): Unit = {

  }
}