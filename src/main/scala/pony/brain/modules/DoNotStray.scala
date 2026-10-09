package pony
package brain
package modules

import scala.reflect.ClassTag

class DoNotStray(universe: Universe) extends DefaultBehaviour[SupportUnit](universe) {
  override protected def wrapBase(t: SupportUnit) = ???
}
