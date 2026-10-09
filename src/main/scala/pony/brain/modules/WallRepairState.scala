package pony
package brain
package modules

private[pony] object WallRepairState {
  sealed trait State
  case object Repairing extends State
  case object Finished  extends State
  case object Failed    extends State
  def apply(workerAlive: Boolean, targetAlive: Boolean, damaged: Boolean, floating: Boolean): State =
    if (!workerAlive) Failed else if (!targetAlive || !damaged) Finished else if (floating) Failed else Repairing
}
