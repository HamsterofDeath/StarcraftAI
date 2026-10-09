package pony
package brain
package modules
package strategy

/**
  * The default campaign behind a Supply Depot wall at the main ramp: the wall stops early rushes, bunkers cover the
  * fields outside it, new command centers fly out to their fields, and the campaign opens the gate when the army
  * launches. The bot's choice whenever its main has a single ramp.
  */
class TerranRampWall(universe: Universe) extends DefaultTerranCampaign(universe) {
  override def name = "Ramp wall"

  /**
    * Above the campaign (1000) when the main base has one ramp: a single choke separates it from the rest of the map
    * (it has a defense line) and it is no island. Otherwise never chosen.
    */
  override def determineScore =
    if (!race.isTerran) -1
    else {
      // terrain does not change: decided once, as soon as the main base is known
      if (oneRampMain.isEmpty) oneRampMain = bases.mainBase.map { main =>
        strategicMap.defenseLineOf(main).isDefined && !mapLayers.isOnIsland(main.mainBuilding.tilePosition)
      }
      if (oneRampMain.contains(true)) 1100 else -1
    }

  private var oneRampMain = Option.empty[Boolean]

  override def usesWallDefense = true
}

final class TerranRampWallPlugin
    extends StrategyPlugin(
      "rampwall",
      "Depot wall at the main ramp, bunkered outer fields, then the massed campaign attack through the gate",
      autoSelectable = true
    )(new TerranRampWall(_))
