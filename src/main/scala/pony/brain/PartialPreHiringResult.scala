package pony
package brain

class PartialPreHiringResult[T <: WrapsUnit](override val canHire: CanHireInfo[T])
  extends PreHiringResult[T] with AtLeastOneSuccess[T] {
  def success = false
}
