package pony
package brain

class SuccessfulPreHiringResult[T <: WrapsUnit](override val canHire: CanHireInfo[T])
  extends PreHiringResult[T] with AtLeastOneSuccess[T] {
  def success = true
}
