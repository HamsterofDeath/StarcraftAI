package pony
package render

import pony.geometry.MapTilePosition

/** Screen geometry for StarCraft's 640x480 window, whose bottom part is covered by the console. */
object CameraPan {
  val ViewWidth    = 640
  val ViewHeight   = 372
  val JumpDistance = 1200
  val MinStep      = 8

  /** The top-left screen pixel that centres `tile`, clamped to the map and aligned to 8 pixels like BWAPI. */
  def screenFor(tile: MapTilePosition, mapTilesWide: Int, mapTilesHigh: Int): (Int, Int) = {
    val x = clamp(tile.x * tileSize + tileSize / 2 - ViewWidth / 2, mapTilesWide * tileSize - ViewWidth)
    val y = clamp(tile.y * tileSize + tileSize / 2 - ViewHeight / 2, mapTilesHigh * tileSize - ViewHeight)
    (x & ~7, y & ~7)
  }

  /** One frame of panning: far targets are jumped to, near ones are approached by a fifth of the gap. */
  def step(from: (Int, Int), to: (Int, Int)): (Int, Int) = {
    val dx = to._1 - from._1
    val dy = to._2 - from._2
    if (math.hypot(dx, dy) > JumpDistance) to
    else (from._1 + towards(dx), from._2 + towards(dy))
  }

  def arrived(at: (Int, Int), target: (Int, Int)): Boolean =
    math.abs(at._1 - target._1) < MinStep && math.abs(at._2 - target._2) < MinStep

  private def towards(delta: Int) = {
    if (math.abs(delta) <= MinStep) delta
    else math.signum(delta) * math.max(MinStep, math.abs(delta) / 5)
  }

  private def clamp(value: Int, max: Int) = value.max(0).min(max.max(0))
}
