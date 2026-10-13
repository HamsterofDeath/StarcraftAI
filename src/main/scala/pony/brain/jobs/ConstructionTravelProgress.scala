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
  private var stillAtSiteSince  = startFrame

  /**
    * At the site the builder fails once it stood `buildTimeout` frames without building, or stayed
    * ConstructionTravelProgress.AtSiteFrames there in all. It used to fail `buildTimeout` frames after it came within
    * reach of the site, walking or not: in game 4 vs Terran on ICCup Lost_Temple at 0cc8d1c the SCV sent to build the
    * first factory walked round a depot to the building's centre, was given up ten frames before it got there, and
    * stood idle on the spot. In 30 games on that map 150 of 343 failed builders still had that order, PlaceBuilding.
    */
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
      stillAtSiteSince = frame
    }
    if (atSite && arrivedAt.isEmpty) {
      arrivedAt = Some(frame)
      stillAtSiteSince = frame
    }
    if (!atSite) arrivedAt = None
    !constructing && (
      if (atSite)
        frame - stillAtSiteSince > buildTimeout || frame - arrivedAt.get > ConstructionTravelProgress.AtSiteFrames
      else frame - lastProgressFrame > 24 * 30
    )
  }
}

private[pony] object ConstructionTravelProgress {

  /** Frames a builder may spend at its site before it fails even while it still moves there. */
  val AtSiteFrames = 24 * 15
}
