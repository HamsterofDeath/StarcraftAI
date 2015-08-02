package pony
package brain

trait Universe {
  def world: DefaultWorld
  def bases: Bases
  def resources: ResourceManager
  def units: UnitManager
}