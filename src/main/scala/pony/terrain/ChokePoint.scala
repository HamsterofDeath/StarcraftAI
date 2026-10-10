package pony
package terrain

import pony.geometry.{CuttingLine, MapTilePosition}

case class ChokePoint(center: MapTilePosition, lines: Seq[CuttingLine], index: Int = -1)
