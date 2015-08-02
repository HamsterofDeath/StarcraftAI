package pony

import bwapi.Game

import scala.collection.mutable.ArrayBuffer

abstract class Order {
  private var myGame:Game = _

  def setGame_!(game: Game):Unit = {
    myGame = game
  }

  def game = myGame

  def queue():Unit
  def renderDebug():Unit
}

object Orders {
  case class Move(unit:Mobile, to:Point)
  case class Gather(unit:SCV, patch:MineralPatch)
  case class Stop(unit:Mobile)
}


class OrderQueue(game: Game) {
  private val queue = ArrayBuffer.empty[Order]

  def queue_!(order: Order):Unit = {
    order.setGame_!(game)
    queue += order
  }

  def debugAll():Unit = {
    queue.foreach(_.renderDebug())
  }

  def issueAll():Unit = {
    queue.foreach(_.queue())
    queue.clear()
  }
}