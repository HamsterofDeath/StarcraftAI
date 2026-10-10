package pony
package brain
package requests

import pony.units.WrapsUnit

trait AtLeastOneSuccess[T <: WrapsUnit] extends PreHiringResult[T] {
  def one = canHire.details.head
}
