package pony
package brain

case class JobIndex[T <: WrapsUnit](e: Employer[T], c: Class[? <: T]) {
  def fitsTo[X <: WrapsUnit](e: Employer[X], u: X) = this.e == e && c.isInstance(u)

  def fitsTo[X <: WrapsUnit](j: UnitWithJob[X]) = e == j.employer && c.isInstance(j.unit)
}
