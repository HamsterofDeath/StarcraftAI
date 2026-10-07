package pony

import java.io.{File, PrintWriter}
import bwapi.Game
import scala.collection.JavaConverters._

/** Only the real BWAPI callbacks write terminal results. A stopped process has no outcome. */
object NativeMatchEvidence {
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
    val completeMap = game.isFlagEnabled(bwapi.Flag.Enum.CompleteMapInformation.getValue)
    try output.println(
      "{\"schema\":1,\"run\":" + quoted(sys.props.getOrElse("twailight.run", "interactive")) +
      ",\"producer\":" + quoted(sys.props.getOrElse("twailight.producer", "unrecorded")) +
      ",\"status\":" + quoted(status) + ",\"winner\":" + winner.map(_.toString).getOrElse("null") +
      ",\"nativeFrame\":" + game.getFrameCount + ",\"selfRace\":" + quoted(game.self().getRace.toString) +
      ",\"selfId\":" + game.self().getID + ",\"selfType\":" + quoted(game.self().getType.toString) +
      ",\"opponents\":" + opponents + ",\"map\":" + quoted(game.mapFileName()) + ",\"mapHash\":" + quoted(game.mapHash()) +
      ",\"completeMapInformation\":" + completeMap + ",\"ordinaryVision\":" + !completeMap +
      ",\"mapInputSha256\":" + quoted(sys.props.getOrElse("twailight.mapInputSha256", "unrecorded")) +
      ",\"configuration\":{\"minFighters\":" + config.minFighters + ",\"armyMinerals\":" + config.armyMinerals +
      ",\"armyGas\":" + config.armyGas + ",\"expansionReserve\":" + config.expansionReserve + "}}")
    finally output.close()
    println("TWAILIGHT_NATIVE_OUTCOME status=" + status + " winner=" + winner + " frame=" + game.getFrameCount)
  }
  def started(game: Game): Unit = write(game, "unfinished", None)
  def ended(game: Game, winner: Boolean): Unit = write(game, if (winner) "win" else "loss", Some(winner))
  def failed(game: Game, error: Throwable): Unit = {
    write(game, "crash", None)
    System.err.println("TWAILIGHT_NATIVE_FAILURE " + error.getClass.getName + ": " + error.getMessage)
  }
}
