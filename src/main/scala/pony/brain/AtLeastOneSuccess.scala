package pony
package brain

trait AtLeastOneSuccess[T <: WrapsUnit] extends PreHiringResult[T] {
  def one = canHire.details.head
}
