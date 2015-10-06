package pony.brain.modules

import pony.brain.{Employer, HasUniverse, UnitJobRequests, Universe}
import pony.{GroundUnit, MapTilePosition, TransporterUnit}

import scala.collection.mutable.ArrayBuffer

class FerryManager(override val universe: Universe) extends HasUniverse {

  private val ferryPlans = ArrayBuffer.empty[FerryPlan]

  private val employer = new Employer[TransporterUnit](universe)

  def requestFerry(forWhat: Set[GroundUnit], to: MapTilePosition, buildNewIfRequired: Boolean = false) = {
    val transporters = 1
    val result = unitManager
                 .request(UnitJobRequests.idleOfType(employer, race.transporterClass, transporters), buildNewIfRequired)
    result.ifNotZero { seq =>

    }
  }

  def onTick(): Unit = {

  }
}

case class FerryPlan(ferry: TransporterUnit, toTransport: Set[GroundUnit], toWhere: MapTilePosition)
