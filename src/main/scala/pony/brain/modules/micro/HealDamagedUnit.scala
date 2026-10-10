package pony
package brain
package modules
package micro

import pony.units.Medic

import scala.reflect.ClassTag

class HealDamagedUnit(universe: Universe) extends DefaultBehaviour[Medic](universe) {
  override protected def wrapBase(t: Medic) = ???
}
