package pony
package brain
package modules

private[pony] object MobileRequestAdmission {
  def accept(result: PreHiringResult[?])(releaseFunding: => Unit): Boolean = {
    if (result.hasAnyMissingRequirements) { releaseFunding; false }
    else true
  }
}
