package pony
package brain

class FailedPreHiringResult[T <: WrapsUnit] extends PreHiringResult[T] {
  def success = false

  def canHire = CanHireInfo.empty
}
