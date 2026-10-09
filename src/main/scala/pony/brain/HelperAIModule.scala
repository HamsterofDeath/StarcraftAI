package pony
package brain

class HelperAIModule[T <: WrapsUnit : Manifest](universe: Universe)
  extends OrderlessAIModule[T](universe) {
  override def onTick_!(): Unit = {}
}
