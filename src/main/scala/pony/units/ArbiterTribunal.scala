package pony
package units

import bwapi.{Unit => APIUnit, _}

class ArbiterTribunal(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
