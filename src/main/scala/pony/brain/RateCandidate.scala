package pony
package brain

trait RateCandidate[T <: WrapsUnit] {
  def giveRating(forThatOne: UnitWithJob[T]): PriorityChain
}
