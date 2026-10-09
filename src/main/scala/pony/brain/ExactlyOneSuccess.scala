package pony
package brain

trait ExactlyOneSuccess[T <: WrapsUnit] extends AtLeastOneSuccess[T] {
  def onlyOne = {
    assert(canHire.details.size == 1)
    canHire.details.head
  }
}
