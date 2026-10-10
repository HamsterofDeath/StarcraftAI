package pony
package units

import bwapi.{Unit => APIUnit, _}

class Egg(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with IsBig with ZergUnit
