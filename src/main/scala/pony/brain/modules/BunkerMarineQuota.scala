package pony
package brain
package modules

/** Indexed dead cargo is not a survivor; visible training and its job are one slot. */
private[pony] object BunkerMarineQuota {
  def missing(
      seats: Int,
      nativeCompleted: Set[Int],
      nativeIncomplete: Set[Int],
      trainingJobs: Int,
      fundedRequests: Seq[Int],
      campaignHeld: Set[Int] = Set.empty,
      obsoleteCargo: Set[Int] = Set.empty
  ): Int =
    (seats - (nativeCompleted -- campaignHeld -- obsoleteCargo).size -
      (nativeIncomplete.size max trainingJobs) - fundedRequests.sum) max 0
}
