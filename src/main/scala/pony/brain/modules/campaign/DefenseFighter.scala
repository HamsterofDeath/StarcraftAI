package pony
package brain
package modules
package campaign

import pony.geometry.MapTilePosition

private[pony] case class DefenseFighter(
    id: Int,
    tile: MapTilePosition,
    campaignAssigned: Boolean = false,
    garrisonReserved: Boolean = false
)
