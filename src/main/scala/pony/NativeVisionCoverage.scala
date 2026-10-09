package pony

/** BWAPI 4.1 emits a final MatchFrame with synthetic complete-map access before MatchEnd. */
private[pony] class NativeVisionCoverage {
  var completeMapDuringPlay                                                                  = false
  var liveSamples                                                                            = 0
  var lastLiveFrame                                                                          = 0
  var terminalSamples                                                                        = 0
  var terminalFrame                                                                          = 0
  var terminalCompleteMap                                                                    = false
  def observe(frame: Int, inGame: Boolean, terminal: Boolean, completeMap: Boolean): Boolean = {
    if (!inGame) false
    else if (terminal) {
      terminalSamples += 1
      terminalFrame = frame
      terminalCompleteMap ||= completeMap
      false
    } else {
      completeMapDuringPlay ||= completeMap
      liveSamples += 1
      lastLiveFrame = lastLiveFrame max frame
      true
    }
  }
  def ordinaryVision = liveSamples > 0 && !completeMapDuringPlay
}
