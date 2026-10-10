package pony
package brain
package budget

trait ResourceApproval {
  lazy val sum = ResourceRequestSum(minerals, gas, supply)
  def minerals: Int
  def gas: Int
  def supply: Int
  def success: Boolean
  def failed           = !success
  def isSuccess        = success
  def assumeSuccessful = {
    assert(success)
    this.asInstanceOf[ResourceApprovalSuccess]
  }
  def ifSuccess[T](body: ResourceApprovalSuccess => T): T
}
