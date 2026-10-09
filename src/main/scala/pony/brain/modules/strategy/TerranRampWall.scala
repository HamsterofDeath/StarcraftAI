package pony
package brain
package modules
package strategy

/**
  * The default campaign behind a Supply Depot wall at the main ramp: the wall stops early rushes, bunkers cover the
  * fields outside it, new command centers fly out to their fields, and the campaign opens the gate when the army
  * launches.
  */
class TerranRampWall(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Ramp wall"

  override def usesWallDefense = true
}

final class TerranRampWallPlugin
    extends StrategyPlugin(
      "rampwall",
      "Depot wall at the main ramp, bunkered outer fields, then the massed campaign attack through the gate",
      autoSelectable = false
    )(new TerranRampWall(_))
