package pony
package units

import bwapi.{Unit => APIUnit, _}

class Irrelevant(unit: APIUnit) extends AnyUnit(unit) {
  override def center = !!!(s"Why did this get called?")
}
