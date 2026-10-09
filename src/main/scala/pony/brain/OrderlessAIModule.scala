package pony
package brain

abstract class OrderlessAIModule[T <: WrapsUnit : Manifest](universe: Universe)
  extends AIModule[T](universe) with Orderless[T] {
  def debugText = ""
}
