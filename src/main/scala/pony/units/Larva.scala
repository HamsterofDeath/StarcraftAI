package pony
package units

import bwapi.{Unit => APIUnit, _}

class Larva(unit: APIUnit)
    extends AnyUnit(unit) with IsSmall with GroundUnit with ZergUnit
