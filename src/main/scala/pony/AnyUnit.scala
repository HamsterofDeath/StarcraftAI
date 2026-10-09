package pony

import bwapi.{Unit => APIUnit, _}

abstract class AnyUnit(val nativeUnit: APIUnit)
    extends WrapsUnit with NiceToString with OrderHistorySupport {}
