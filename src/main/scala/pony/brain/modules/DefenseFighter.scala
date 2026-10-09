package pony
package brain
package modules

private[pony] case class DefenseFighter(
    id: Int,
    tile: MapTilePosition,
    campaignAssigned: Boolean = false,
    garrisonReserved: Boolean = false
)
