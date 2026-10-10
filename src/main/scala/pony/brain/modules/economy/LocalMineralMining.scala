package pony
package brain
package modules
package economy

/** Cargo carried from another field cannot prove that this patch is being worked. */
private[pony] object LocalMineralMining {
  def observed(mining: Boolean, assignedPatch: Int, nativeTarget: Option[Int], nearby: Boolean) =
    mining && nativeTarget.contains(assignedPatch) && nearby
}
