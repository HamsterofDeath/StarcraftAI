package pony

import pony.AttackPriorities.Highest
import pony.brain.modules.ProvideExpansions
import pony.brain.{HasUniverse, Universe}

import scala.util.{Failure, Success, Try}

class DebugHelper(main: MainAI) extends AIPlugIn with HasUniverse {

  main.listen_!(new AIAPI {
    override def world: DefaultWorld = main.world

    override def onSendText(s: String): Unit = {
      super.onSendText(s)
      val words = s.split(' ').toList
      Try(words match {
        case command :: params =>
          command match {
            case "log" | "l" =>
              params match {
                case List(logLevel) =>

                  setLogLevel_!(logLevel match {
                    case "0" => LogLevels.LogOff
                    case "1" => LogLevels.LogError
                    case "2" => LogLevels.LogWarn
                    case "3" => LogLevels.LogInfo
                    case "4" => LogLevels.LogDebug
                    case "5" => LogLevels.LogTrace
                    case _ => !!!(logLevel)
                  })
                case _ => usage(command, "one log level from 0 to 5")
              }
            case "expand" | "e" =>
              params match {
                case List(mineralsId) =>
                  val patch = world.resourceAnalyzer.groups.find(_.patchId.toString == mineralsId)
                              .get
                  main.brain.pluginByType[ProvideExpansions].forceExpand(patch)
                case _ => usage(command, "one mineral patch id")
              }

            case "speed" | "s" =>
              params match {
                case List(integer) =>
                  universe.world.debugger.speed(integer.toInt)
                case _ => usage(command, "one speed value")
              }
            case "debugoff" | "doff" =>
              universe.world.debugger.off()
            case "debugon" | "don" =>
              universe.world.debugger.on()
            case "debugmoff" | "dmoff" =>
              universe.world.debugger.fullOff()
            case "debugmon" | "dmon" =>
              universe.world.debugger.fullOn()
            case "debug" | "d" =>
              params match {
                case List(id) =>
                  ownUnits.allKnownUnits.find(_.unitIdText == id).foreach(debugUnit)
                  enemies.allKnownUnits.find(_.unitIdText == id).foreach(debugUnit)
                case _ => usage(command, "one unit id")
              }
            case "attack" | "a" =>
              val target = params match {
                case List(x, id) if x == "m" || x == "minerals" =>
                  world.resourceAnalyzer.groups.find(_.patchId.toString == id).map(_.center)

                case List(x, id) if x == "c" || x == "choke" =>
                  strategicMap.domains.find(_._1.index.toString == id).map(_._1.center)

                case List(x, id) if x == "n" || x == "narrow" =>
                  strategicMap.narrowPoints.find(_.index.toString == id).map(_.where)

                case List(x, id) if x == "u" || x == "unit" =>
                  enemies.allMobilesAndBuildings.find(_.unitIdText == id).map(_.centerTile)
                case _ => usage(command, "a target kind (m, c, n or u) and an id")
              }
              target.foreach { where =>
                main.brain.universe.worldDominationPlan.initiateAttack(where, Highest)
              }
          }
        case Nil =>

      }) match {
        case Success(_) =>
        case Failure(ex) =>
          ex.printStackTrace()
      }
    }
  })

  override def lazyWorld: DefaultWorld = main.world

  override def universe: Universe = main.universe

  override protected def tickPlugIn(): Unit = {
    // nop
  }

  private def usage(command: String, expected: String): Nothing =
    !!!(s"Debug command '$command' expects $expected")

  private def debugUnit(wrapsUnit: WrapsUnit): Unit = {
    info(s"user requested inspection of $wrapsUnit")
  }
}
