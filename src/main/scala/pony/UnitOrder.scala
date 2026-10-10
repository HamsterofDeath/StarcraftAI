package pony

import pony.render.Renderer
import pony.units.{OrderHistorySupport, WrapsUnit}

import bwapi.Game

import scala.compiletime.uninitialized

abstract class UnitOrder {
  private var myGame: Game      = uninitialized
  private var locks             = 0
  private var forceAllowRepeats = false

  def forceRepetition = forceAllowRepeats

  def forceRepeat_!(forceRepeats: Boolean) = {
    forceAllowRepeats = forceRepeats
    this
  }

  def lockTicks = locks

  def obsolete = !myUnit.isInGame

  def setGame_!(game: Game): Unit = {
    myGame = game
  }

  def game = myGame

  def isNoop = false

  def myUnit: WrapsUnit

  def record(): Unit = {
    myUnit match {
      case his: OrderHistorySupport =>
        his.trackOrder(this)
      case _ =>
    }
  }

  def issueOrderToGame(): Unit

  def renderDebug(renderer: Renderer): Unit

  def lockingFor_!(ticks: Int) = {
    locks = ticks
    this
  }

}
