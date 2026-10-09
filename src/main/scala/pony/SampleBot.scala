package pony

import bwapi.Text.Size
import bwapi.{BWClient, DefaultBWListener, Game, Player, UnitType, Unit => APIUnit}

import scala.compiletime.uninitialized
import scala.jdk.CollectionConverters._

/** JBWAPI's minimal sample bot: it trains SCVs and sends idle workers to the closest mineral field. */
object SampleBot {
  def main(args: Array[String]): Unit = new SampleBot().run()
}

class SampleBot extends DefaultBWListener {
  private var client: BWClient = uninitialized
  private var game: Game       = uninitialized
  private var self: Player     = uninitialized

  def run(): Unit = {
    client = new BWClient(this)
    client.startGame()
  }

  override def onUnitCreate(unit: APIUnit): Unit = {
    println(s"New unit ${unit.getType}")
  }

  override def onStart(): Unit = {
    game = client.getGame
    self = game.self()
  }

  override def onFrame(): Unit = {
    game.setTextSize(Size.Default)
    game.drawTextScreen(10, 10, s"Playing as ${self.getName} - ${self.getRace}")

    val units = new StringBuilder("My units:\n")
    self.getUnits.asScala.foreach { myUnit =>
      units.append(myUnit.getType).append(" ").append(myUnit.getTilePosition).append("\n")

      if (myUnit.getType == UnitType.Terran_Command_Center && self.minerals() >= 50) {
        myUnit.train(UnitType.Terran_SCV)
      }

      if (myUnit.getType.isWorker && myUnit.isIdle) {
        val minerals = game.neutral().getUnits.asScala.filter(_.getType.isMineralField)
        minerals.minByOption(myUnit.getDistance(_)).foreach(myUnit.gather(_, false))
      }
    }

    game.drawTextScreen(10, 25, units.toString)
  }
}
