package pony

import pony.terrain.MapPlan

import bwapi.{BWClient, BWClientConfiguration, BWEventListener, Position, Unit => NUnit}

/**
  * Created by HoD on 01.08.2015.
  */
object Controller {

  def main(args: Array[String]): Unit = {
    try {
      hookOnToBroodwar(ConcoctedAI.concoct)
    } catch {
      case ex: Throwable =>
        ex.printStackTrace()
        System.exit(1)
    }
  }

  def hookOnToBroodwar(aiGenerator: (DefaultWorld) => AIAPI) = {
    var clientRef: BWClient = null

    var ai                = Option.empty[AIAPI]
    var world             = Option.empty[DefaultWorld]
    var frameClock        = new NativeFrameClock
    var repeatedCallbacks = 0
    val listener          = new BWEventListener {
      override def onUnitCreate(unit: NUnit): Unit = {
        world.foreach(_.onUnitCreate(unit))
      }

      override def onFrame(): Unit = {
        val liveGame = clientRef.getGame
        if (world.isEmpty || !liveGame.isInGame) return
        if (!NativeMatchEvidence.observeLiveVision(liveGame)) return
        // Paused native games can deliver repeated callbacks without simulation progress.
        if (!frameClock.advance(liveGame.getFrameCount)) {
          repeatedCallbacks += 1
          if (repeatedCallbacks == 100) {
            NativeMatchEvidence.trace(
              "native-stall",
              s"nativeFrame=${liveGame.getFrameCount} paused=${liveGame.isPaused} inGame=${liveGame.isInGame} fps=${liveGame.getFPS}"
            )
            if (liveGame.isInGame && liveGame.isPaused) {
              liveGame.resumeGame()
              NativeMatchEvidence.trace("native-resume", "resume paused local match")
            }
          }
          return
        }
        repeatedCallbacks = 0
        TickCounter.tickCount += 1
        ai.foreach(_.onTickOnApi())
        // every 30 game seconds
        if (TickCounter.tickCount % 720 == 0) {
          val game = clientRef.getGame
          NativeMatchEvidence.trace(
            "economy-heartbeat",
            s"nativeFrame=${game.getFrameCount} paused=${game.isPaused} fps=${game.getFPS} " +
              NativeMatchEvidence.economy(game)
          )
        }
      }

      override def onUnitShow(unit: NUnit): Unit = {
        world.foreach(_.onUnitShow(unit))
      }

      override def onUnitDiscover(unit: NUnit): Unit = {
        world.foreach(_.onUnitDiscover(unit))
      }

      override def onUnitComplete(unit: NUnit): Unit = {
        world.foreach(_.onUnitComplete(unit))
        if (unit.getPlayer == clientRef.getGame.self() && unit.getType.isResourceDepot)
          NativeMatchEvidence.trace("base-completed", s"id=${unit.getID} tile=${unit.getTilePosition}")
      }

      override def onUnitEvade(unit: NUnit): Unit = {
        world.foreach(_.onUnitEvade(unit))
      }

      override def onSendText(s: String): Unit = {
        ai.foreach(_.onSendText(s))
      }

      override def onEnd(b: Boolean): Unit = {
        NativeMatchEvidence.ended(clientRef.getGame, b)
        ai = None
        world = None
        if (MapPlan.isSession && MapPlan.isLast(NativeMatchEvidence.currentGame)) {
          // the session is complete; the runner stops StarCraft once every result is written
          System.exit(0)
        }
      }

      override def onSaveGame(s: String): Unit = {}

      override def onPlayerDropped(player: bwapi.Player): Unit = {
        world.foreach(_.onPlayerDropped(player))
        ai.foreach(_.onPlayerDropped(player))
      }

      override def onUnitHide(unit: NUnit): Unit = {
        world.foreach(_.onUnitHide(unit))
      }

      override def onUnitRenegade(unit: NUnit): Unit = {
        world.foreach(_.onUnitRenegade(unit))
      }

      override def onStart(): Unit = {
        try {
          TickCounter.tickCount = 0
          frameClock = new NativeFrameClock
          repeatedCallbacks = 0
          NativeMatchEvidence.started(clientRef.getGame)
          if (MapPlan.isSession) {
            NativeMatchEvidence.trace(
              "session-game",
              s"game=${NativeMatchEvidence.currentGame} of ${MapPlan.maps.size} map=${clientRef.getGame.mapName()}"
            )
          }
          val headless   = sys.props.getOrElse("twailight.headless", "false").toBoolean
          val localSpeed = sys.props.getOrElse("twailight.localSpeed", "0").toInt
          clientRef.getGame.setGUI(!headless)
          clientRef.getGame.setLocalSpeed(localSpeed)
          NativeMatchEvidence.trace("native-rendering", s"gui=${!headless} localSpeed=$localSpeed")
          if (sys.props.get("twailight.traceMap").contains("true"))
            NativeMatchEvidence.traceStartArea(clientRef.getGame)
          clientRef.getGame.enableFlag(bwapi.Flag.UserInput)
          // calibration only: the bot sees everything, and the game reports the enemy's real income and spending
          if (sys.props.get("twailight.completeMap").contains("true"))
            clientRef.getGame.enableFlag(bwapi.Flag.CompleteMapInformation)
          val w = DefaultWorld.spawn(clientRef.getGame)
          world = Some(w)
          ai = Some(aiGenerator(w))
        } catch {
          case ex: Throwable =>
            NativeMatchEvidence.failed(clientRef.getGame, ex)
            throw ex
        }
      }

      override def onPlayerLeft(player: bwapi.Player): Unit = {
        world.foreach(_.onPlayerLeft(player))
        ai.foreach(_.onPlayerLeft(player))
      }

      override def onNukeDetect(position: Position): Unit = {
        world.foreach(_.onNukeDetect(position))
      }

      override def onUnitDestroy(unit: NUnit): Unit = {
        world.foreach(_.onUnitDestroy(unit))
      }

      override def onUnitMorph(unit: NUnit): Unit = {
        world.foreach(_.onUnitMorph(unit))
      }

      override def onReceiveText(player: bwapi.Player, s: String): Unit = {
        ai.foreach(_.onReceiveText(player, s))
      }

    }

    clientRef = new BWClient(listener)
    // a warm session keeps the client connected while BWAPI restarts the game with the next map
    clientRef.startGame(new BWClientConfiguration.Builder().withAutoContinue(MapPlan.isSession).build())

  }
}
