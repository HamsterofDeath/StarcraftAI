package pony
package terrain

import pony.geometry.MapTilePosition

case class PotentialDomain(coveredOnLand: Seq[ResourceArea], needsToControl: Seq[MapTilePosition])
