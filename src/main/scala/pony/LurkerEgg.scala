package pony

import bwapi.{Unit => APIUnit, _}

class LurkerEgg(unit: APIUnit)
    extends AnyUnit(unit) with GroundUnit with IsBig with ZergUnit
