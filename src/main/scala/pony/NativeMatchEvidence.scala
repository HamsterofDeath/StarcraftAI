package pony

import java.io.{File, PrintWriter}
import bwapi.Game
import scala.collection.JavaConverters._

/** Only the real BWAPI callbacks write terminal results. A stopped process has no outcome. */
object NativeMatchEvidence {
  private var completeMapObservedDuringPlay = false
  private var liveFlagSamples = 0
  private var startupFailed = false
  private var lastLiveFrame = 0
  private var initialState = "{}"
  def observeLiveVision(game: Game): Unit = {
    completeMapObservedDuringPlay ||= game.isFlagEnabled(bwapi.Flag.Enum.CompleteMapInformation.getValue)
    liveFlagSamples += 1
    lastLiveFrame = lastLiveFrame max game.getFrameCount
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
    val completeMap = completeMapObservedDuringPlay
    try output.println(
      "{\"schema\":1,\"run\":" + quoted(sys.props.getOrElse("twailight.run", "interactive")) +
      ",\"producer\":" + quoted(sys.props.getOrElse("twailight.producer", "unrecorded")) +
      ",\"status\":" + quoted(status) + ",\"winner\":" + winner.map(_.toString).getOrElse("null") +
      ",\"nativeFrame\":" + lastLiveFrame + ",\"callbackNativeFrame\":" + game.getFrameCount +
      ",\"selfRace\":" + quoted(game.self().getRace.toString) +
      ",\"selfId\":" + game.self().getID + ",\"selfType\":" + quoted(game.self().getType.toString) +
      ",\"opponents\":" + opponents + ",\"map\":" + quoted(game.mapFileName()) + ",\"mapHash\":" + quoted(game.mapHash()) +
      ",\"completeMapInformation\":" + completeMap + ",\"ordinaryVision\":" + !completeMap +
      ",\"liveFlagSamples\":" + liveFlagSamples +
      ",\"callbackCompleteMapInformation\":" + game.isFlagEnabled(bwapi.Flag.Enum.CompleteMapInformation.getValue) +
      ",\"mapInputSha256\":" + quoted(sys.props.getOrElse("twailight.mapInputSha256", "unrecorded")) +
      ",\"initialState\":" + initialState +
      ",\"configuration\":{\"minFighters\":" + config.minFighters + ",\"armyMinerals\":" + config.armyMinerals +
      ",\"armyGas\":" + config.armyGas + ",\"expansionReserve\":" + config.expansionReserve + "}}")
    finally output.close()
    println("TWAILIGHT_NATIVE_OUTCOME status=" + status + " winner=" + winner + " frame=" + game.getFrameCount)
  }
  def started(game: Game): Unit = {
    completeMapObservedDuringPlay = false
    liveFlagSamples = 0
    startupFailed = false
    lastLiveFrame = 0
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
