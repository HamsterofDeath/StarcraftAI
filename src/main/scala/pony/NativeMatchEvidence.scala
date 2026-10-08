package pony

import java.io.{File, PrintWriter}
import bwapi.Game
import scala.collection.JavaConverters._

/** BWAPI 4.1 emits a final MatchFrame with synthetic complete-map access before MatchEnd. */
private[pony] class NativeVisionCoverage {
  var completeMapDuringPlay = false
  var liveSamples = 0
  var lastLiveFrame = 0
  var terminalSamples = 0
  var terminalFrame = 0
  var terminalCompleteMap = false
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

/** Only the real BWAPI callbacks write terminal results. A stopped process has no outcome. */
object NativeMatchEvidence {
  private var vision = new NativeVisionCoverage
  private var startupFailed = false
  private var initialState = "{}"
  def observeLiveVision(game: Game): Boolean = {
    if (!game.isInGame) return false
    val player = game.self()
    // These exact native victory/defeat fields are exported with the final synthetic flag batch.
    val terminal = player != null && (player.isDefeated || player.isVictorious)
    val complete = game.isFlagEnabled(bwapi.Flag.Enum.CompleteMapInformation.getValue)
    val wasInvalid = vision.completeMapDuringPlay
    val playing = vision.observe(game.getFrameCount, true, terminal, complete)
    if (!playing && vision.terminalSamples == 1)
      trace("native-terminal-frame", s"nativeFrame=${game.getFrameCount} defeated=${player.isDefeated} victorious=${player.isVictorious} completeMap=$complete")
    if (playing && complete && !wasInvalid)
      trace("invalid-live-vision", s"nativeFrame=${game.getFrameCount} inGame=${game.isInGame} paused=${game.isPaused}")
    playing
  }
  def trace(event: String, detail: String): Unit =
    println("TWAILIGHT_CAMPAIGN frame=" + pony.tickCount + " event=" + event + " detail=" + detail)
  private def quoted(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  private def write(game: Game, status: String, winner: Option[Boolean]): Unit = {
    val directory = new File(sys.props.getOrElse("twailight.resultDirectory", "log"))
    directory.mkdirs()
    val output = new PrintWriter(new File(directory, "native-result.json"))
    val opponents = game.enemies().asScala.map { p =>
      "{\"id\":" + p.getID + ",\"name\":" + quoted(p.getName) + ",\"race\":" + quoted(p.getRace.toString) +
        ",\"type\":" + quoted(p.getType.toString) + "}"
    }.mkString("[", ",", "]")
    val config = pony.brain.modules.TerranCampaignConfig.load()
    // BWAPI exposes end-state flags after the game closes. Acceptance uses samples taken during play.
    val completeMap = vision.completeMapDuringPlay
    try output.println(
      "{\"schema\":1,\"run\":" + quoted(sys.props.getOrElse("twailight.run", "interactive")) +
      ",\"producer\":" + quoted(sys.props.getOrElse("twailight.producer", "unrecorded")) +
      ",\"status\":" + quoted(status) + ",\"winner\":" + winner.map(_.toString).getOrElse("null") +
      ",\"nativeFrame\":" + vision.lastLiveFrame + ",\"callbackNativeFrame\":" + game.getFrameCount +
      ",\"selfRace\":" + quoted(game.self().getRace.toString) +
      ",\"selfId\":" + game.self().getID + ",\"selfType\":" + quoted(game.self().getType.toString) +
      ",\"opponents\":" + opponents + ",\"map\":" + quoted(game.mapFileName()) + ",\"mapHash\":" + quoted(game.mapHash()) +
      ",\"completeMapInformation\":" + completeMap + ",\"ordinaryVision\":" + vision.ordinaryVision +
      ",\"liveFlagSamples\":" + vision.liveSamples +
      ",\"terminalFlagSamples\":" + vision.terminalSamples + ",\"terminalNativeFrame\":" + vision.terminalFrame +
      ",\"terminalCompleteMapInformation\":" + vision.terminalCompleteMap +
      ",\"callbackCompleteMapInformation\":" + game.isFlagEnabled(bwapi.Flag.Enum.CompleteMapInformation.getValue) +
      ",\"mapInputSha256\":" + quoted(sys.props.getOrElse("twailight.mapInputSha256", "unrecorded")) +
      ",\"renderingEnabled\":" + !sys.props.getOrElse("twailight.headless", "false").toBoolean +
      ",\"initialState\":" + initialState +
      ",\"configuration\":{\"minFighters\":" + config.minFighters + ",\"armyMinerals\":" + config.armyMinerals +
      ",\"armyGas\":" + config.armyGas + ",\"expansionReserve\":" + config.expansionReserve +
      ",\"bankMinerals\":" + config.bankMinerals + ",\"bankGas\":" + config.bankGas + "}}")
    finally output.close()
    println("TWAILIGHT_NATIVE_OUTCOME status=" + status + " winner=" + winner + " frame=" + game.getFrameCount)
  }
  def started(game: Game): Unit = {
    vision = new NativeVisionCoverage
    startupFailed = false
    val start = game.self().getStartLocation
    initialState = "{\"minerals\":" + game.self().minerals() + ",\"gas\":" + game.self().gas() +
      ",\"startTile\":[" + start.getX + "," + start.getY + "],\"ownUnits\":" +
      game.self().getUnits.asScala.map { unit =>
        "{\"id\":" + unit.getID + ",\"type\":" + quoted(unit.getType.toString) + "}"
      }.mkString("[", ",", "]") + "}"
    observeLiveVision(game)
    write(game, "unfinished", None)
  }
  def ended(game: Game, winner: Boolean): Unit =
    write(game, if (startupFailed) "crash" else if (winner) "win" else "loss", Some(winner))
  def failed(game: Game, error: Throwable): Unit = {
    startupFailed = true
    write(game, "crash", None)
    System.err.println("TWAILIGHT_NATIVE_FAILURE " + error.getClass.getName + ": " + error.getMessage)
  }
}
