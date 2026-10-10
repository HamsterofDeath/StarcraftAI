package pony
package units

case class AllUnits(own: Units, other: Units) {
  def byNativeId(id: Int) = {
    own.byId(id).orElse(other.byId(id))
  }
}
