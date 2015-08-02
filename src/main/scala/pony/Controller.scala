package pony

import bwapi.{Unit => NUnit, Position, Player, BWEventListener, Mirror}

/**
 * Created by HoD on 01.08.2015.
 */
object Controller {

  def hookOnToBroodwar(aiGenerator:(DefaultWorld) => AIAPI) = {
    val mirror = new Mirror

    var ai = Option.empty[AIAPI]
    var world = Option.empty[DefaultWorld]
    val listener = new BWEventListener {
      override def onUnitCreate(unit: NUnit): Unit = {
        world.foreach(_.onUnitCreate(unit))
      }

      override def onFrame(): Unit = {
        ai.foreach(_.onTick())
      }

      override def onUnitShow(unit: NUnit): Unit = {
        world.foreach(_.onUnitShow(unit))
      }

      override def onUnitDiscover(unit: NUnit): Unit = {
        world.foreach(_.onUnitDiscover(unit))
      }

      override def onUnitComplete(unit: NUnit): Unit =  {
        world.foreach(_.onUnitComplete(unit))
      }

      override def onUnitEvade(unit: NUnit): Unit = {
        world.foreach(_.onUnitEvade(unit))
      }

      override def onSendText(s: String): Unit = {
        ai.foreach(_.onSendText(s))
      }

      override def onEnd(b: Boolean): Unit = {
        ai = None
        world = None
      }

      override def onSaveGame(s: String): Unit = {

      }

      override def onPlayerDropped(player: Player): Unit = {
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
        val w = DefaultWorld.spawn(mirror.getGame)
        world = Some(w)
        ai = Some(aiGenerator(w))
      }

      override def onPlayerLeft(player: Player): Unit = {
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

      override def onReceiveText(player: Player, s: String): Unit = {
        ai.foreach(_.onReceiveText(player, s))
      }

    }

    mirror.getModule.setEventListener(listener)
    mirror.startGame()

  }

  def main(args: Array[String]) {
    hookOnToBroodwar(ConcoctedAI.concoct)
  }
}
