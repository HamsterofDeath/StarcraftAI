package pony
package brain
package modules

/** Saturation is a milestone: lending a miner to construction must not cancel a funded expansion. */
private[pony] class TerranEconomicProgress {
  private var startingField                                             = Option.empty[Int]
  private var saturated                                                 = false
  private var secondEstablished                                         = false
  def observe(start: Option[Int], fields: Seq[MiningFieldStatus]): Unit = {
    if (startingField.isEmpty) startingField = start
    saturated ||= startingField.exists(id => fields.exists(f => f.id == id && f.saturated))
    secondEstablished ||= saturated && startingField.exists(id =>
      fields.exists(f => f.id != id && f.operational)
    )
  }
  def startingFieldSaturated = saturated
  def secondBaseEstablished  = secondEstablished
}
