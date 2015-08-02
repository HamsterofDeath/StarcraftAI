package pony
package brain

import bwapi.Color
import pony.{DefaultWorld, MainBuilding, Order}

import scala.collection.mutable.ArrayBuffer

class TwilightSparkle(world: DefaultWorld) {

  private val bases = new Bases(world)

  def queueOrdersForTick():Unit = {
    if (world.isFirstTick) {
      bases.findMainBase()
    }

    bases.tick()

  }
}

class Bases(world:DefaultWorld) {
  def tick(): Unit = {
    bases.foreach(_.tick())
  }

  val bases = ArrayBuffer.empty[Base]

  def findMainBase():Unit = {
    world.myUnits.firstByType[MainBuilding].foreach {bases += new Base(world, _)}
  }
}

class Base(world: DefaultWorld, mainBuilding: MainBuilding) {
  def tick(): Unit = {
    world.mineralPatches.groups.foreach { mpg =>
      mpg.patches.foreach { mp =>
        world.debugger.render.in_!(Color.Green).writeText(mp.position, mpg.patchId)
      }
    }
  }

  val myPatch = world.mineralPatches.nearestTo(mainBuilding.position)

  info(
    s"""
       |Found base/minerals $mainBuilding: $myPatch
     """.stripMargin)
}

