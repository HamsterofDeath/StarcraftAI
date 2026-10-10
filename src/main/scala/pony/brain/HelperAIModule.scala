package pony
package brain

import pony.units.WrapsUnit

import scala.reflect.ClassTag

class HelperAIModule[T <: WrapsUnit: ClassTag](universe: Universe)
    extends OrderlessAIModule[T](universe) {
  override def onTick_!(): Unit = {}
}
