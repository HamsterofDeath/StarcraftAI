package pony
package units

import pony.brain._

trait Detector extends WrapsUnit with HasLazyVals {
  private val myDetectionArea = oncePerTick {
    geoHelper.circle(centerTile, detectionRadius)
  }

  def detectionRadius: Int

  def detectionArea = myDetectionArea.get
}
