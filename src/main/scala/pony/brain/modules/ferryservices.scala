package pony.brain.modules

import pony.brain.{Employer, HasUniverse, UnitJobRequests, Universe}
import pony.{GroundUnit, MapTilePosition, TransporterUnit}

import scala.collection.mutable.ArrayBuffer

class FerryManager(override val universe: Universe) extends HasUniverse {

  private val ferryPlans = ArrayBuffer.empty[FerryPlan]

  private val employer = new Employer[TransporterUnit](universe)

  def planFor(ferry: TransporterUnit) = {
    ferryPlans.find(_.ferry == ferry).foreach(_.onTick())
  }

  def requestFerry(forWhat: GroundUnit, to: MapTilePosition, buildNewIfRequired: Boolean = false) = {
    ferryPlans.find(_.covers(forWhat, to))
    .orElse(requestFerries(forWhat.toSet, to, buildNewIfRequired).headOption)

  }

  private def requestFerries(forWhat: Set[GroundUnit], to: MapTilePosition, buildNewIfRequired: Boolean = false) = {
    val groups = ArrayBuffer.empty[FerryCargoBuilder]
    val remaining = forWhat.toBuffer

    while (remaining.nonEmpty) {
      var open = new FerryCargoBuilder
      remaining.iterator.takeWhile(open.canAdd).foreach(open.add_!)
      remaining --= open.cargo
      groups += open
    }

    val result = unitManager
                 .request(UnitJobRequests.idleOfType(employer, race.transporterClass, groups.size), buildNewIfRequired)
    val newPlans = result.ifNotZero(seq => {
      seq.zip(groups).map {
        case (transporter, cargo) => FerryPlan(transporter, cargo.cargo.toSet, to)
      }
    }, Nil)
    ferryPlans ++= newPlans
    newPlans
  }

  def onTick(): Unit = {

  }
}

class FerryCargoBuilder {
  private val myCargo = ArrayBuffer.empty[GroundUnit]
  private var left    = 8

  def cargo = myCargo.toSeq

  def canAdd(g: GroundUnit) = left >= g.transportSize

  def add_!(g: GroundUnit) = {
    assert(canAdd(g))
    myCargo += g
    left -= g.transportSize
  }
}

case class FerryPlan(ferry: TransporterUnit, toTransport: Set[GroundUnit], toWhere: MapTilePosition) {
  def onTick(): Unit = {

  }

  def covers(forWhat: GroundUnit, to: MapTilePosition) = {
    toTransport(forWhat) && toWhere == to
  }

  def unloadedLeft = toTransport.exists(_.unLoaded)
  assert(toTransport.map(_.transportSize).sum <= 8, s"Too many units for single transport: $toTransport")

}
