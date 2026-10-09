package pony

import bwapi.{Unit => APIUnit, _}

class CyberneticsCore(unit: APIUnit) extends AnyUnit(unit) with Building with Upgrader
