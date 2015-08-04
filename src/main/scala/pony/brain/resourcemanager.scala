package pony
package brain

class ResourceManager(override val universe: Universe) extends HasUniverse {
  private var currentResources = Resources(0, 0, 0, 0, 0)
  def tick(): Unit = {
    currentResources = universe.world.currentResources
  }
}

trait ResourceRequest {
  def amount: Int
}
case class MineralsRequest(amount: Int) extends ResourceRequest
case class GasRequest(amount: Int) extends ResourceRequest
case class SupplyRequest(amount: Int) extends ResourceRequest
case class ResourceRequests(requests: Seq[ResourceRequest], priority: Priority)


