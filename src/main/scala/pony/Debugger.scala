package pony

import bwapi.{Color, Game}

class Debugger(game: Game, world: DefaultWorld) {
  def isRendering = logLevel.includes(LogLevels.LogTrace)

  val renderer = new Renderer(game, Color.Green)
  private var debugging     = false
  private var fullDebugMode = false
  private var countTicks    = 0

  def isFullDebug = fullDebugMode

  def off(): Unit = {
    debugging = false
  }

  def fullOn(): Unit = {
    on()
    fullDebugMode = true
  }

  def on(): Unit = {
    debugging = true
  }

  def fullOff(): Unit = {
    fullDebugMode = false
  }

  def isDebugging = debugging || fullDebugMode

  def speed(int: Int): Unit = {
    game.setLocalSpeed(int)
  }

  def fastest(): Unit = {
    game.setLocalSpeed(0)
  }

  def slowMotion(): Unit = {
    game.setLocalSpeed(255)
  }

  def debugRender(whatToDo: Renderer => Any): Unit = {
    countTicks += 1
    if (debugging && countTicks > 5 && isRendering) {
      world.addPostTickAction {
        whatToDo(renderer)
      }
    }
  }

  def chat(msg: String): Unit = {
    game.sendText(msg)
  }

  def revealMap(): Unit = {
    game.setRevealAll()
  }

}
