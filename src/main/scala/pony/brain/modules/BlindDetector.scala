package pony
package brain
package modules

import scala.reflect.ClassTag

class BlindDetector(universe: Universe) extends DefaultBehaviour[Medic](universe) {
  override protected def wrapBase(t: Medic) = ???
}
