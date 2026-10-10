package pony
package units

import bwapi.{Unit => APIUnit, _}

class Observer(unit: APIUnit)
    extends AnyUnit(unit) with MobileDetector with Mechanic with IsSmall with AirUnit with PermaCloak
