package pony

import pony.terrain.MapPlan

import java.io.{File, PrintWriter}
import bwapi.Game
import scala.jdk.CollectionConverters._

/** Only the real BWAPI callbacks write terminal results. A stopped process has no outcome. */
object NativeMatchEvidence {
  private var vision        = new NativeVisionCoverage
  private var startupFailed = false
  private var initialState  = "{}"
  private var gameNumber    = 0
  private var liveGame      = Option.empty[Game]

  private val refusals = scala.collection.mutable.HashMap.empty[String, Int]

  /**
    * Traces the terrain around our start as text, one map-row per tile row: '.' walkable and buildable, ',' walkable
    * only, '~' partly walkable, '#' not walkable, 'm' minerals, 'g' a geyser, 'C' our start command center. Lets a
    * headless run show why workers cannot reach a site.
    */
  def traceStartArea(game: Game, radiusX: Int = 22, radiusY: Int = 16): Unit = {
    val start                              = game.self().getStartLocation
    val (w, h)                             = (game.mapWidth, game.mapHeight)
    val marks                              = scala.collection.mutable.HashMap.empty[(Int, Int), Char]
    def mark(u: bwapi.Unit, c: Char): Unit = {
      val t = u.getTilePosition
      for (dx <- 0 until u.getType.tileWidth; dy <- 0 until u.getType.tileHeight) marks((t.x + dx, t.y + dy)) = c
    }
    game.getStaticMinerals.asScala.foreach(mark(_, 'm'))
    game.getStaticGeysers.asScala.foreach(mark(_, 'g'))
    for (dx <- 0 until 4; dy <- 0 until 3) marks((start.x + dx, start.y + dy)) = 'C'
    val x0 = (start.x - radiusX).max(0)
    val x1 = (start.x + radiusX).min(w - 1)
    for (y <- (start.y - radiusY).max(0) to (start.y + radiusY).min(h - 1)) {
      val row = (x0 to x1).map { x =>
        marks.getOrElse(
          (x, y), {
            val walkable =
              (for (wx <- 0 until 4; wy <- 0 until 4) yield game.isWalkable(x * 4 + wx, y * 4 + wy)).count(identity)
            if (walkable == 0) '#'
            else if (walkable < 16) '~'
            else if (game.isBuildable(x, y)) '.'
            else ','
          }
        )
      }.mkString
      trace("map-row", f"y=$y%3d x0=$x0%3d $row")
    }
  }

  /** Counts refusals of one build; true for the 1st, 10th, 100th, ... so repeated refusals stay readable. */
  def firstRefusals(key: String): Boolean = {
    val n = refusals.getOrElse(key, 0) + 1
    refusals(key) = n
    n == 1 || n == 10 || n % 100 == 0
  }

  /**
    * Why BWAPI may refuse `builder` building `kind` at `tile`: JBWAPI does not expose BWAPI's last error, so this reports
    * the checks behind it.
    */
  def buildDiagnosis(builder: bwapi.Unit, tile: bwapi.TilePosition, kind: bwapi.UnitType): String =
    liveGame.fold("game=unknown") { g =>
      val (w, h)      = (kind.tileWidth, kind.tileHeight)
      val unbuildable = for {
        dx <- 0 until w
        dy <- 0 until h
        if !g.isBuildable(tile.x + dx, tile.y + dy, true)
      } yield s"(${tile.x + dx},${tile.y + dy})"
      val corner = new bwapi.TilePosition(tile.x + w, tile.y + h)
      val inWay  = g
        .getUnitsInRectangle(tile.toPosition, corner.toPosition)
        .asScala
        .filter(u => u.getID != builder.getID && !u.isFlying && !u.isLoaded)
        .map(u => s"${u.getID}:${u.getType.toString.replaceAll("^[A-Za-z]+_", "")}:${u.getOrder}")
      val center = new bwapi.Position(tile.x * 32 + w * 16, tile.y * 32 + h * 16)
      s"builderCanBuild=${builder.canBuild(kind)} builderCanBuildThere=${builder.canBuild(kind, tile)} " +
        s"canMake=${g.canMake(kind, builder)} canBuildHere=${g.canBuildHere(tile, kind, builder)} " +
        s"explored=${g.canBuildHere(tile, kind, builder, true)} interruptible=${builder.isInterruptible} " +
        s"order=${builder.getOrder} minerals=${g.self().minerals} path=${builder.hasPath(center)} " +
        s"unbuildable=${unbuildable.mkString(",")} inWay=${inWay.mkString(",")}"
    }

  /** 1-based number of the current game in this bot process; warm sessions play several. */
  def currentGame: Int                       = gameNumber
  def observeLiveVision(game: Game): Boolean = {
    if (!game.isInGame) return false
    liveGame = Some(game)
    leaveOnRequest(game)
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
    * A runner out of time asks the bot to leave by a `leave-request` file next to the result: StarCraft writes the
    * replay only for a game it leaves, and a game stopped at the time limit left nothing to study. The game stays
    * unfinished, not lost.
    */
  private def leaveOnRequest(game: Game): Unit =
    if (!leaveRequested && game.getFrameCount % 48 == 0 && new File(resultDirectory, "leave-request").exists) {
      leaveRequested = true
      trace("leave-requested", s"nativeFrame=${game.getFrameCount}")
      game.leaveGame()
    }
  private var leaveRequested  = false
  private def resultDirectory = new File(sys.props.getOrElse("twailight.resultDirectory", "log"))

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

  /**
    * Surviving units other than buildings and their summed hit points and shields, for micro scenarios; in total and by
    * type, which the battle battery learns from.
    */
  private def forces(units: Iterable[bwapi.Unit]) = {
    val alive =
      units.filter(u => u.exists && !u.getType.isBuilding && u.getType != bwapi.UnitType.Special_Start_Location)
        .toVector
    def summary(of: Vector[bwapi.Unit]) =
      "{\"units\":" + of.size + ",\"durability\":" + of.iterator.map(u => u.getHitPoints + u.getShields).sum
    val byType =
      alive.groupBy(_.getType.toString).toVector.sortBy(_._1).map((t, of) => quoted(t) + ":" + summary(of) + "}")
    summary(alive) + ",\"byType\":" + byType.mkString("{", ",", "}") + "}"
  }

  private def quoted(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  private def write(game: Game, status: String, winner: Option[Boolean]): Unit = {
    val directory = resultDirectory
    directory.mkdirs()
    val output    = new PrintWriter(new File(directory, resultFileName))
    val opponents = game.enemies().asScala.map { p =>
      "{\"id\":" + p.getID + ",\"name\":" + quoted(p.getName) + ",\"race\":" + quoted(p.getRace.toString) +
        ",\"type\":" + quoted(p.getType.toString) + "}"
    }.mkString("[", ",", "]")
    val config = pony.brain.modules.campaign.TerranCampaignConfig.load()
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
    leaveRequested = false
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
    if (leaveRequested) write(game, "unfinished", None)
    else write(game, if (startupFailed) "crash" else if (winner) "win" else "loss", Some(winner))
  def failed(game: Game, error: Throwable): Unit = {
    startupFailed = true
    write(game, "crash", None)
    System.err.println("TWAILIGHT_NATIVE_FAILURE " + error.getClass.getName + ": " + error.getMessage)
  }
}
