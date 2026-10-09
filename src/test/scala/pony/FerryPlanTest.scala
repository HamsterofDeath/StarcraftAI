package pony

import java.lang.reflect.{InvocationHandler, Method, Proxy}

import org.specs2.Specification
import org.specs2.matcher.MustMatchers
import pony.brain.modules.FerryPlan

class FerryPlanTest extends Specification with MustMatchers {
  def is =
    s2"""
       |A full ferry rejects replacing one slot with four without changing cargo $rejectOverCapacity
       |A full ferry accepts replacing four slots with four $replaceEqualSize
       |Mixed candidates replace only cargo that leaves enough capacity $chooseFeasibleCargo
       """.stripMargin

  private val targetArea = new Grid2D(1, 1, collection.immutable.BitSet.empty)

  // Ferry planning needs only unit state; no native game or new mocking dependency is required.
  private def unitState[T](unitClass: Class[T], name: String,
                           slots: Int, x: Int, onGround: Boolean): T = {
    val state = new InvocationHandler {
      override def invoke(proxy: AnyRef, method: Method, arguments: Array[AnyRef]): AnyRef = {
        method.getName match {
          case "transportSize" => Int.box(slots)
          case "currentTile" => MapTilePosition(x, 0)
          case "currentArea" => None
          case "onGround" => Boolean.box(onGround)
          case "currentTick" => Int.box(0)
          case "toString" => name
          case "hashCode" => Int.box(System.identityHashCode(proxy))
          case "equals" => Boolean.box(proxy eq arguments(0))
          case unexpected => throw new UnsupportedOperationException(unexpected)
        }
      }
    }
    Proxy.newProxyInstance(unitClass.getClassLoader, Array[Class[?]](unitClass), state)
    .asInstanceOf[T]
  }

  private def cargo(name: String, slots: Int, x: Int, onGround: Boolean = true) =
    unitState(classOf[GroundUnit], name, slots, x, onGround)

  private def plan(initial: GroundUnit) = {
    val ferry = unitState(classOf[TransporterUnit], "ferry", 8, 0, onGround = false)
    new FerryPlan(ferry, initial, MapTilePosition(50, 0), Some(targetArea))
  }

  def rejectOverCapacity = {
    val queued = cargo("queued small unit", 1, 20)
    val incoming = cargo("incoming large unit", 4, 1)
    val ferryPlan = plan(queued)
    (1 to 7).foreach { index =>
      ferryPlan.withMore_!(cargo(s"loaded small unit $index", 1, index, onGround = false))
    }
    val before = ferryPlan.toTransport.toSet

    val replaced = ferryPlan.replaceQueuedUnitIfPossible_!(incoming)

    (replaced, ferryPlan.toTransport.toSet, ferryPlan.takenSpace) === (false, before, 8)
  }

  def replaceEqualSize = {
    val queued = cargo("queued large unit", 4, 20)
    val loaded = cargo("loaded large unit", 4, 2, onGround = false)
    val incoming = cargo("incoming large unit", 4, 1)
    val ferryPlan = plan(queued).withMore_!(loaded)

    val replaced = ferryPlan.replaceQueuedUnitIfPossible_!(incoming)

    (replaced, ferryPlan.toTransport.toSet, ferryPlan.takenSpace) ===
      (true, Set(loaded, incoming), 8)
  }

  def chooseFeasibleCargo = {
    val farthestSmall = cargo("farthest small unit", 1, 30)
    val nearerLarge = cargo("nearer large unit", 4, 20)
    val incoming = cargo("incoming large unit", 4, 1)
    val loaded = (1 to 3).map { index =>
      cargo(s"loaded small unit $index", 1, index, onGround = false)
    }
    val ferryPlan = plan(farthestSmall).withMore_!(nearerLarge)
    loaded.foreach(ferryPlan.withMore_!)

    val replaced = ferryPlan.replaceQueuedUnitIfPossible_!(incoming)

    (replaced, ferryPlan.toTransport.toSet, ferryPlan.takenSpace) ===
      (true, loaded.toSet ++ Set(farthestSmall, incoming), 8)
  }
}
