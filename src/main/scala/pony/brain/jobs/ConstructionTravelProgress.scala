package pony
package brain
package jobs

import pony.geometry.MapTilePosition

/** A pathfinding Move is ordinary builder travel, not a failed native Build command. */
private[pony] class ConstructionTravelProgress(
    startFrame: Int,
    startPosition: MapTilePosition,
    target: Option[MapTilePosition] = None
) {
  private var lastPosition      = startPosition
  private var lastProgressFrame = startFrame
  private var closest           = target.map(startPosition.distanceSquaredTo)
  private var arrivedAt         = Option.empty[Int]
  def failed(
      frame: Int,
      position: MapTilePosition,
      atSite: Boolean,
      constructing: Boolean,
      buildTimeout: Int
  ): Boolean = {
    if (position != lastPosition) {
      val distance = target.map(position.distanceSquaredTo)
      if (distance.isEmpty || distance.zip(closest).exists { case (now, best) => now < best }) {
        closest = distance
        lastProgressFrame = frame
      }
      lastPosition = position
    }
    if (atSite && arrivedAt.isEmpty) arrivedAt = Some(frame)
    if (!atSite) arrivedAt = None
    !constructing && (if (atSite) frame - arrivedAt.get > buildTimeout else frame - lastProgressFrame > 24 * 30)
  }
}
