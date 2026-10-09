package pony

import bwapi._

object TypeMapping {

  private val class2UnitType: Map[Class[? <: WrapsUnit], UnitType] = UnitWrapper.class2UnitType

  // TODO return cached copies to save native calls
  def unitTypeOf[T <: WrapsUnit](c: Class[? <: T]) = class2UnitType(c)
}
