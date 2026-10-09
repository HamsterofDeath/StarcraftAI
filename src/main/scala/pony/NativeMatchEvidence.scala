package pony

import java.io.{File, PrintWriter}
import bwapi.Game
import scala.jdk.CollectionConverters._

/** Only the real BWAPI callbacks write terminal results. A stopped process has no outcome. */
object NativeMatchEvidence {
  private var vision        = new NativeVisionCoverage
  private var startupFailed = false
  private var initialState  = "{}"
  private var gameNumber    = 0

  /** 1-based number of the current game in this bot process; warm sessions play several. */
  def currentGame: Int                       = gameNumber
  def observeLiveVision(game: Game): Boolean = {
    if (!game.isInGame) return false
    val player = game.self()
    // These exact native victory/defeat fields are exported with the final synthetic flag batch.
    val terminal   = player != null && (player.isDefeated || player.isVictorious)
    val complete   = game.isFlagEnabled(bwapi.Flag.CompleteMapInformation)
    val wasInvalid = vision.completeMapDuringPlay
    val playing    = vision.observe(game.getFrameCount, true, terminal, complete)
    if (!playing && vision.terminalSamples == 1)
      trace(
        "native-terminal-frame",
        s"nativeFrame=${game.getFrameCount} defeated=${player.isDefeated} victorious=${player.isVictorious} completeMap=$complete"
      )
    if (playing && complete && !wasInvalid)
      trace("invalid-live-vision", s"nativeFrame=${game.getFrameCount} inGame=${game.isInGame} paused=${game.isPaused}")
    playing
  }
  def trace(event: String, detail: String): Unit =
    println("TWAILIGHT_CAMPAIGN frame=" + TickCounter.tickCount + " event=" + event + " detail=" + detail)

  /**
    * Bank, income so far, supply (in whole units), what the SCVs are doing, bases and fighters: one line for economy
    * analysis.
    */
  def economy(game: Game): String = {
    val self                               = game.self()
    val units                              = self.getUnits.asScala.filter(_.exists).toVector
    val scvs                               = units.filter(_.getType == bwapi.UnitType.Terran_SCV)
    def scvsThat(p: bwapi.Unit => Boolean) = scvs.count(p)
    def built(t: bwapi.UnitType)           = {
      val all = units.filter(_.getType == t)
      s"${all.count(_.isCompleted)}/${all.size}"
    }
    val fighters = units.count(u => !u.getType.isBuilding && !u.getType.isWorker && u.isCompleted)
    s"minerals=${self.minerals} gas=${self.gas} gathered=${self.gatheredMinerals}/${self.gatheredGas} " +
      s"supply=${self.supplyUsed / 2}/${self.supplyTotal / 2} scvs=${scvs.size} " +
      s"onMinerals=${scvsThat(_.isGatheringMinerals)} onGas=${scvsThat(_.isGatheringGas)} " +
      s"constructing=${scvsThat(_.isConstructing)} repairing=${scvsThat(_.isRepairing)} idle=${scvsThat(_.isIdle)} " +
      s"ccs=${built(bwapi.UnitType.Terran_Command_Center)} refineries=${built(bwapi.UnitType.Terran_Refinery)} " +
      s"fighters=$fighters"
  }

  /** Surviving units other than buildings and their summed hit points and shields, for micro scenarios. */
  private def forces(units: Iterable[bwapi.Unit]) = {
    val alive =
      units.filter(u => u.exists && !u.getType.isBuilding && u.getType != bwapi.UnitType.Special_Start_Location)
    "{\"units\":" + alive.size + ",\"durability\":" + alive.iterator.map(u => u.getHitPoints + u.getShields).sum + "}"
  }

  private def quoted(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  private def write(game: Game, status: String, winner: Option[Boolean]): Unit = {
    val directory = new File(sys.props.getOrElse("twailight.resultDirectory", "log"))
    directory.mkdirs()
    val output    = new PrintWriter(new File(directory, resultFileName))
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
          ",\"opponents\":" + opponents + ",\"map\":" + quoted(game.mapFileName()) + ",\"mapHash\":" +
          quoted(game.mapHash()) +
          ",\"completeMapInformation\":" + completeMap + ",\"ordinaryVision\":" + vision.ordinaryVision +
          ",\"liveFlagSamples\":" + vision.liveSamples +
          ",\"terminalFlagSamples\":" + vision.terminalSamples + ",\"terminalNativeFrame\":" + vision.terminalFrame +
          ",\"terminalCompleteMapInformation\":" + vision.terminalCompleteMap +
          ",\"callbackCompleteMapInformation\":" + game.isFlagEnabled(bwapi.Flag.CompleteMapInformation) +
          ",\"mapInputSha256\":" + quoted(sys.props.getOrElse("twailight.mapInputSha256", "unrecorded")) +
          ",\"renderingEnabled\":" + !sys.props.getOrElse("twailight.headless", "false").toBoolean +
          ",\"initialState\":" + initialState +
          ",\"game\":" + gameNumber +
          ",\"selfForces\":" + forces(game.self().getUnits.asScala) +
          ",\"enemyForces\":" + forces(game.enemies().asScala.flatMap(_.getUnits.asScala)) +
          ",\"configuration\":{\"minFighters\":" + config.minFighters + ",\"armyMinerals\":" + config.armyMinerals +
          ",\"armyGas\":" + config.armyGas + ",\"expansionReserve\":" + config.expansionReserve +
          ",\"bankMinerals\":" + config.bankMinerals + ",\"bankGas\":" + config.bankGas + "}}"
      )
    finally output.close()
    println("TWAILIGHT_NATIVE_OUTCOME status=" + status + " winner=" + winner + " frame=" + game.getFrameCount)
  }
  private def resultFileName = if (MapPlan.isSession) s"native-result-$gameNumber.json" else "native-result.json"

  def started(game: Game): Unit = {
    gameNumber += 1
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
