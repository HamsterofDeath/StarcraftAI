package pony

import bwapi.{BWEventListener, Mirror, Position, Player => NPlayer, Unit => NUnit}

/**
  * Created by HoD on 01.08.2015.
  */
object Controller {

  def main(args: Array[String]): Unit = {
    try {
      hookOnToBroodwar(ConcoctedAI.concoct)
    }
    catch {
      case ex: Throwable => ex.printStackTrace()
        System.exit(1)
    }
  }

  def hookOnToBroodwar(aiGenerator: (DefaultWorld) => AIAPI) = {
    val mirror = new Mirror

    var ai = Option.empty[AIAPI]
    var world = Option.empty[DefaultWorld]
    var frameClock = new NativeFrameClock
    var repeatedCallbacks = 0
    val listener = new BWEventListener {
      override def onUnitCreate(unit: NUnit): Unit = {
        world.foreach(_.onUnitCreate(unit))
      }

      override def onFrame(): Unit = {
        val liveGame = mirror.getGame
        if (world.isEmpty || !liveGame.isInGame) return
        if (!NativeMatchEvidence.observeLiveVision(liveGame)) return
        // Paused native games can deliver repeated callbacks without simulation progress.
        if (!frameClock.advance(liveGame.getFrameCount)) {
          repeatedCallbacks += 1
          if (repeatedCallbacks == 100) {
            NativeMatchEvidence.trace("native-stall",
              s"nativeFrame=${liveGame.getFrameCount} paused=${liveGame.isPaused} inGame=${liveGame.isInGame} fps=${liveGame.getFPS}")
            if (liveGame.isInGame && liveGame.isPaused) {
              liveGame.resumeGame()
              NativeMatchEvidence.trace("native-resume", "resume paused local match")
            }
          }
          return
        }
        repeatedCallbacks = 0
        pony.tickCount += 1
        ai.foreach(_.onTickOnApi())
        if (pony.tickCount % 2400 == 0) {
          val game = mirror.getGame
          val own = game.self().getUnits
          val ownUnits = scala.jdk.CollectionConverters.asScalaBufferConverter(own).asScala
          NativeMatchEvidence.trace("economy-heartbeat",
            s"nativeFrame=${game.getFrameCount} paused=${game.isPaused} inGame=${game.isInGame} fps=${game.getFPS} minerals=${game.self().minerals()} gas=${game.self().gas()} supply=${game.self().supplyUsed()}/${game.self().supplyTotal()} scvs=${ownUnits.count(_.getType == bwapi.UnitType.Terran_SCV)} depots=${ownUnits.count(_.getType == bwapi.UnitType.Terran_Command_Center)} completeMap=${game.isFlagEnabled(bwapi.Flag.Enum.CompleteMapInformation.getValue)}")
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
        if (unit.getPlayer == mirror.getGame.self() && unit.getType.isResourceDepot)
          NativeMatchEvidence.trace("base-completed", s"id=${unit.getID} tile=${unit.getTilePosition}")
      }

      override def onUnitEvade(unit: NUnit): Unit = {
        world.foreach(_.onUnitEvade(unit))
      }

      override def onSendText(s: String): Unit = {
        ai.foreach(_.onSendText(s))
      }

      override def onEnd(b: Boolean): Unit = {
        NativeMatchEvidence.ended(mirror.getGame, b)
        ai = None
        world = None
      }

      override def onSaveGame(s: String): Unit = {

      }

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
          pony.tickCount = 0
          frameClock = new NativeFrameClock
          repeatedCallbacks = 0
          NativeMatchEvidence.started(mirror.getGame)
          val headless = sys.props.getOrElse("twailight.headless", "false").toBoolean
          mirror.getGame.setGUI(!headless)
          mirror.getGame.setLocalSpeed(0)
          NativeMatchEvidence.trace("native-rendering", s"gui=${!headless} localSpeed=0")
          mirror.getGame.enableFlag(bwapi.Flag.Enum.UserInput.getValue)
          val w = DefaultWorld.spawn(mirror.getGame)
          world = Some(w)
          ai = Some(aiGenerator(w))
        }
        catch {
          case ex: Throwable =>
            NativeMatchEvidence.failed(mirror.getGame, ex)
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

    mirror.getModule.setEventListener(listener)
    mirror.startGame()

  }
}

private[pony] class NativeFrameClock {
  private var previous = -1
  def advance(nativeFrame: Int): Boolean = {
    if (nativeFrame <= previous) false
    else { previous = nativeFrame; true }
  }
}
