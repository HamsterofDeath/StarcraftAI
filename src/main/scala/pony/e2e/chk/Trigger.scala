package pony.e2e.chk

import java.nio.{ByteBuffer, ByteOrder}

/** One TRIG entry: up to 16 conditions and 64 actions, run for the listed players (0-based). */
final case class Trigger(players: Seq[Int], conditions: Seq[Trigger.Condition], actions: Seq[Trigger.Action]) {
  require(conditions.size <= 16 && actions.size <= 64)

  def bytes: Array[Byte] = {
    val out = ByteBuffer.allocate(Trigger.Size).order(ByteOrder.LITTLE_ENDIAN)
    conditions.zipWithIndex.foreach { case (c, i) =>
      out.position(i * 20)
      out.putInt(c.location).putInt(c.player).putInt(c.amount).putShort(c.unit.toShort)
      out.put(c.comparison.toByte).put(c.kind.toByte).put(c.resource.toByte)
      out.put((if (c.unit != 0) Trigger.UnitTypeUsed else 0).toByte)
    }
    actions.zipWithIndex.foreach { case (a, i) =>
      out.position(320 + i * 32)
      out.putInt(a.location).putInt(a.text).putInt(0).putInt(a.millis).putInt(a.player).putInt(a.second)
      out.putShort(a.unit.toShort).put(a.kind.toByte).put(a.modifier.toByte)
      out.put((if (a.unit != 0) Trigger.UnitTypeUsed else 0).toByte)
    }
    players.foreach(p => out.put(320 + 64 * 32 + 4 + p, 1.toByte))
    out.array()
  }
}

object Trigger {
  val Size         = 2400
  val UnitTypeUsed = 0x10

  /** Location numbers in triggers are 1-based; this is location 64, which always covers the whole map. */
  val Anywhere = 64
  val Men      = 230
  val AnyUnit  = 229

  val AtLeast = 0
  val AtMost  = 1

  final case class Condition(
      kind: Int,
      player: Int = 0,
      unit: Int = 0,
      comparison: Int = 0,
      amount: Int = 0,
      location: Int = 0,
      resource: Int = 0
  )

  final case class Action(
      kind: Int,
      player: Int = 0,
      second: Int = 0,
      unit: Int = 0,
      modifier: Int = 0,
      location: Int = 0,
      text: Int = 0,
      millis: Int = 0
  )

  def always = Condition(22)

  def elapsedSeconds(atLeast: Int) = Condition(12, comparison = AtLeast, amount = atLeast)

  def commandsAtMost(player: Int, unit: Int, amount: Int) =
    Condition(2, player = player, unit = unit, comparison = AtMost, amount = amount)

  def victory = Action(1)

  def defeat = Action(2)

  def preserve = Action(3)

  /** Runs one of the built-in AI scripts, named by its four-letter id such as "Suic". */
  def runAiScript(script: String) =
    Action(15, second = ByteBuffer.wrap(script.getBytes("ASCII")).order(ByteOrder.LITTLE_ENDIAN).getInt(0))

  /** Sets `player`'s minerals and gas to `amount`. */
  def setResources(player: Int, amount: Int) =
    Action(26, player = player, second = amount, unit = OreAndGas, modifier = SetTo)

  private val OreAndGas = 2
  private val SetTo     = 7

  /** Orders `unit` of `player` inside `from` to `orderType` (0 move, 1 patrol, 2 attack) towards `to`. */
  def order(player: Int, unit: Int, from: Int, to: Int, orderType: Int) =
    Action(46, player = player, second = to, unit = unit, modifier = orderType, location = from)
}
