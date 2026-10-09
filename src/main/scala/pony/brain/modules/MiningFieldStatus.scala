package pony
package brain
package modules

case class MiningFieldStatus(
    id: Int,
    capacity: Int,
    assigned: Int,
    working: Int,
    landedCompleted: Boolean
) {
  def saturated   = landedCompleted && capacity > 0 && assigned >= capacity
  def operational = landedCompleted && capacity > 0 && working > 0
}
