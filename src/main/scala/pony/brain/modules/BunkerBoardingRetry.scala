package pony
package brain
package modules

/** Retry refused boarding, but leave a progressing native approach untouched. */
private[pony] class BunkerBoardingRetry {
  private var lastAttempt  = -1000
  private var lastProgress = 0
  private var lastPosition = Option.empty[MapTilePosition]
  def issue(
      frame: Int,
      position: MapTilePosition,
      loaded: Boolean,
      headingToBunker: Boolean,
      moving: Boolean
  ): Boolean = {
    if (!lastPosition.contains(position)) { lastPosition = Some(position); lastProgress = frame }
    if (loaded || frame - lastAttempt < 12 || (headingToBunker && moving && frame - lastProgress < 120)) false
    else { lastAttempt = frame; true }
  }
}
