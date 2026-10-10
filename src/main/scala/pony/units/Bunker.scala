package pony
package units

import pony.combat.ArmedBuildingCoveringGroundAndAir

import bwapi.{Unit => APIUnit, _}

class Bunker(unit: APIUnit)
    extends AnyUnit(unit) with TerranBuilding with ArmedBuildingCoveringGroundAndAir
