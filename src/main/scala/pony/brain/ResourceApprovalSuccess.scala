package pony
package brain

case class ResourceApprovalSuccess(
    minerals: Int,
    gas: Int,
    supply: Int,
    uniqueId: ResourceApprovalId
) extends ResourceApproval {
  def success = true

  override def ifSuccess[T](thenDo: (ResourceApprovalSuccess) => T): T = thenDo(this)
}

object ResourceApprovalSuccess {
  private var counter = 0

  def apply(sums: ResourceRequestSum): ResourceApprovalSuccess = {
    counter += 1
    ResourceApprovalSuccess(
      sums.minerals,
      sums.gas,
      sums.supply,
      ResourceApprovalId(counter)
    )
  }
}
