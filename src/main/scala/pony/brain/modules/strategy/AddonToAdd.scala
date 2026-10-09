package pony
package brain
package modules
package strategy

final case class AddonToAdd(addon: Class[? <: Addon], requestNewBuildings: Boolean)(active: => Boolean) {
  def isActive = active
}
