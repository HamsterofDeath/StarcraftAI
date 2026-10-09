package pony

/**
  * Chooses what the camera shows. A focus is kept for at least `minDwellFrames` unless a candidate beats it by
  * `interruptMargin`, so a fight interrupts a calm shot immediately while calm shots never flicker. A candidate with
  * the same reason within `followRadius` tiles is the same scene that moved, so the camera follows it.
  */
final class AutoCameraDirector(minDwellFrames: Int, interruptMargin: Int, followRadius: Int = 12) {
  private var current    = Option.empty[CameraFocus]
  private var shownSince = 0

  def focus: Option[CameraFocus] = current

  def consider(frame: Int, candidates: Seq[CameraFocus]): Option[CameraFocus] = {
    candidates.maxByOption(_.score).foreach { best =>
      current match {
        case Some(shown) if shown.reason == best.reason && shown.tile.distanceTo(best.tile) <= followRadius =>
          current = Some(best)
        case Some(shown) if frame - shownSince < minDwellFrames && best.score < shown.score + interruptMargin =>
        case _                                                                                                =>
          current = Some(best)
          shownSince = frame
      }
    }
    current
  }
}

object AutoCameraDirector {

  /** The point with the most other points within `radius` tiles, and that count including itself. */
  def densest(points: Seq[MapTilePosition], radius: Int): Option[(MapTilePosition, Int)] = {
    val squared = radius * radius
    points.map(p => p -> points.count(_.distanceSquaredTo(p) <= squared)).maxByOption(_._2)
  }
}
