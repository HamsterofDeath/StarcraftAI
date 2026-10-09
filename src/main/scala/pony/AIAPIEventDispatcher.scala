package pony

import scala.collection.mutable.ArrayBuffer

trait AIAPIEventDispatcher extends AIAPI {
  private val receivers = ArrayBuffer.empty[AIAPI]

  def listen_!(aiApi: AIAPI): Unit = {
    receivers += aiApi
  }

  override def onReceiveText(player: bwapi.Player, s: String): Unit = {
    super.onReceiveText(player, s)
    receivers.foreach(_.onReceiveText(player, s))
  }

  override def onPlayerLeft(player: bwapi.Player): Unit = {
    super.onPlayerLeft(player)
    receivers.foreach(_.onPlayerLeft(player))
  }

  override def onPlayerDropped(player: bwapi.Player): Unit = {
    super.onPlayerDropped(player)
    receivers.foreach(_.onPlayerDropped(player))
  }

  override def onSendText(s: String): Unit = {
    super.onSendText(s)
    receivers.foreach(_.onSendText(s))
  }
}
