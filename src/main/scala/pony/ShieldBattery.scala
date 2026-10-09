package pony

import bwapi.{Unit => APIUnit, _}

class ShieldBattery(unit: APIUnit) extends AnyUnit(unit) with Building with ShieldCharger
