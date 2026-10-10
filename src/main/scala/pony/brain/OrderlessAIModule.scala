package pony
package brain

import pony.units.WrapsUnit

import scala.reflect.ClassTag

abstract class OrderlessAIModule[T <: WrapsUnit: ClassTag](universe: Universe)
    extends AIModule[T](universe) with Orderless[T] {
  def debugText = ""
}
