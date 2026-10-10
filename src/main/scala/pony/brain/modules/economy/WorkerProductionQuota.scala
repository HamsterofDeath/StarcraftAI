package pony
package brain
package modules
package economy

/** A funded factory job and its visible unfinished SCV describe the same production slot. */
private[pony] object WorkerProductionQuota {
  def missing(
      target: Int,
      completed: Int,
      incomplete: Int,
      reservedTraining: Int,
      nativeTraining: Int,
      requests: Seq[Int]
  ): Int = {
    val production = incomplete max (reservedTraining max nativeTraining)
    (target - completed - production - requests.sum) max 0
  }
}
