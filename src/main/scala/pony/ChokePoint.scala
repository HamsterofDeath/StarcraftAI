package pony

case class ChokePoint(center: MapTilePosition, lines: Seq[CuttingLine], index: Int = -1)
