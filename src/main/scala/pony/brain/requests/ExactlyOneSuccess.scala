package pony
package brain
package requests

import pony.units.WrapsUnit

trait ExactlyOneSuccess[T <: WrapsUnit] extends AtLeastOneSuccess[T] {
  def onlyOne = {
    assert(canHire.details.size == 1)
    canHire.details.head
  }
}
