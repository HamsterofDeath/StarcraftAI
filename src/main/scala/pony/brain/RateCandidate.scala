package pony
package brain

import pony.brain.jobs.UnitWithJob
import pony.units.WrapsUnit

trait RateCandidate[T <: WrapsUnit] {
  def giveRating(forThatOne: UnitWithJob[T]): PriorityChain
}
