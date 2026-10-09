package pony

import bwapi.{Unit => APIUnit, _}

class Irrelevant(unit: APIUnit) extends AnyUnit(unit) {
  override def center = !!!(s"Why did this get called?")
}
