package pony
package brain

object ResourceApprovalFail extends ResourceApproval {
  override def minerals = 0

  override def gas = 0

  override def supply = 0

  override def success = false

  override def ifSuccess[T](body: (ResourceApprovalSuccess) => T): T = {
    // sorry
    null.asInstanceOf[T]
  }

  override def toString = "Not enough resources"
}
