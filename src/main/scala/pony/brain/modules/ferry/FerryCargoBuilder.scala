package pony
package brain.modules
package ferry

import pony.units.GroundUnit

import scala.collection.mutable.ArrayBuffer

class FerryCargoBuilder {
  private val myCargo = ArrayBuffer.empty[GroundUnit]
  private var left    = 8

  def cargo = myCargo.immutableView

  def add_!(g: GroundUnit) = {
    assert(canAdd(g))
    myCargo += g
    left -= g.transportSize
  }

  def canAdd(g: GroundUnit) = left >= g.transportSize
}
