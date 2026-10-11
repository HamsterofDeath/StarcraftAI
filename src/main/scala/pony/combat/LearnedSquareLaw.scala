package pony
package combat

import bwapi.UnitType
import bwapi.UnitType._

/**
  * The square law learned from 400 battles of a battery (seed 1, bot 266c265) with prior 2.0, written by
  * scripts/battery_fit.py --scala: regenerate it rather than edit it. Medics fire a small stand-in for their healing.
  */
private[combat] object LearnedSquareLaw {
  val Battles = 400
  val Scale   = 4.0429
  val Bias    = 1.1739

  /** Damage a second of the first type against the second, after armor and size. */
  val Dps: Map[(UnitType, UnitType), Double] = Map(
    (Terran_Marine, Protoss_Zealot) -> 8.503,
    (Terran_Marine, Protoss_Dragoon) -> 6.492,
    (Terran_Marine, Protoss_Archon) -> 8.211,
    (Terran_Marine, Protoss_Scout) -> 10.404,
    (Terran_Marine, Protoss_Corsair) -> 9.378,
    (Terran_Firebat, Protoss_Zealot) -> 17.271,
    (Terran_Firebat, Protoss_Dragoon) -> 2.647,
    (Terran_Firebat, Protoss_Archon) -> 4.582,
    (Terran_Medic, Protoss_Zealot) -> 1.001,
    (Terran_Medic, Protoss_Dragoon) -> 1.043,
    (Terran_Medic, Protoss_Archon) -> 1.006,
    (Terran_Medic, Protoss_Scout) -> 1.038,
    (Terran_Medic, Protoss_Corsair) -> 0.992,
    (Terran_Vulture, Protoss_Zealot) -> 20.073,
    (Terran_Vulture, Protoss_Dragoon) -> 3.249,
    (Terran_Vulture, Protoss_Archon) -> 4.354,
    (Terran_Goliath, Protoss_Zealot) -> 12.129,
    (Terran_Goliath, Protoss_Dragoon) -> 12.979,
    (Terran_Goliath, Protoss_Archon) -> 13.850,
    (Terran_Goliath, Protoss_Scout) -> 22.590,
    (Terran_Goliath, Protoss_Corsair) -> 15.071,
    (Terran_Siege_Tank_Tank_Mode, Protoss_Zealot) -> 10.130,
    (Terran_Siege_Tank_Siege_Mode, Protoss_Zealot) -> 10.130,
    (Terran_Siege_Tank_Tank_Mode, Protoss_Dragoon) -> 18.621,
    (Terran_Siege_Tank_Siege_Mode, Protoss_Dragoon) -> 18.621,
    (Terran_Siege_Tank_Tank_Mode, Protoss_Archon) -> 20.190,
    (Terran_Siege_Tank_Siege_Mode, Protoss_Archon) -> 20.190,
    (Terran_Wraith, Protoss_Zealot) -> 6.115,
    (Terran_Wraith, Protoss_Dragoon) -> 5.321,
    (Terran_Wraith, Protoss_Archon) -> 6.099,
    (Terran_Wraith, Protoss_Scout) -> 16.422,
    (Terran_Wraith, Protoss_Corsair) -> 11.946,
    (Terran_Battlecruiser, Protoss_Zealot) -> 21.647,
    (Terran_Battlecruiser, Protoss_Dragoon) -> 19.441,
    (Terran_Battlecruiser, Protoss_Archon) -> 19.664,
    (Terran_Battlecruiser, Protoss_Scout) -> 18.119,
    (Terran_Battlecruiser, Protoss_Corsair) -> 19.123,
    (Protoss_Zealot, Terran_Marine) -> 16.172,
    (Protoss_Zealot, Terran_Firebat) -> 13.259,
    (Protoss_Zealot, Terran_Medic) -> 15.044,
    (Protoss_Zealot, Terran_Vulture) -> 13.856,
    (Protoss_Zealot, Terran_Goliath) -> 13.761,
    (Protoss_Zealot, Terran_Siege_Tank_Tank_Mode) -> 12.785,
    (Protoss_Zealot, Terran_Siege_Tank_Siege_Mode) -> 12.785,
    (Protoss_Dragoon, Terran_Marine) -> 8.536,
    (Protoss_Dragoon, Terran_Firebat) -> 6.157,
    (Protoss_Dragoon, Terran_Medic) -> 6.617,
    (Protoss_Dragoon, Terran_Vulture) -> 11.910,
    (Protoss_Dragoon, Terran_Goliath) -> 13.357,
    (Protoss_Dragoon, Terran_Siege_Tank_Tank_Mode) -> 15.846,
    (Protoss_Dragoon, Terran_Siege_Tank_Siege_Mode) -> 15.846,
    (Protoss_Dragoon, Terran_Wraith) -> 18.256,
    (Protoss_Dragoon, Terran_Battlecruiser) -> 14.294,
    (Protoss_Archon, Terran_Marine) -> 33.347,
    (Protoss_Archon, Terran_Firebat) -> 34.165,
    (Protoss_Archon, Terran_Medic) -> 34.468,
    (Protoss_Archon, Terran_Vulture) -> 31.199,
    (Protoss_Archon, Terran_Goliath) -> 33.880,
    (Protoss_Archon, Terran_Siege_Tank_Tank_Mode) -> 32.751,
    (Protoss_Archon, Terran_Siege_Tank_Siege_Mode) -> 32.751,
    (Protoss_Archon, Terran_Wraith) -> 37.808,
    (Protoss_Archon, Terran_Battlecruiser) -> 33.748,
    (Protoss_Scout, Terran_Marine) -> 6.281,
    (Protoss_Scout, Terran_Firebat) -> 5.767,
    (Protoss_Scout, Terran_Medic) -> 5.396,
    (Protoss_Scout, Terran_Vulture) -> 7.818,
    (Protoss_Scout, Terran_Goliath) -> 5.560,
    (Protoss_Scout, Terran_Siege_Tank_Tank_Mode) -> 5.887,
    (Protoss_Scout, Terran_Siege_Tank_Siege_Mode) -> 5.887,
    (Protoss_Scout, Terran_Wraith) -> 33.583,
    (Protoss_Scout, Terran_Battlecruiser) -> 23.707,
    (Protoss_Corsair, Terran_Wraith) -> 18.421,
    (Protoss_Corsair, Terran_Battlecruiser) -> 5.798
  )

  /** The hit points and shields a type stands for. */
  val Durability: Map[UnitType, Double] = Map(
    Terran_Marine -> 36.2,
    Terran_Firebat -> 50.9,
    Terran_Medic -> 71.2,
    Terran_Vulture -> 78.1,
    Terran_Goliath -> 122.3,
    Terran_Siege_Tank_Tank_Mode -> 154.3,
    Terran_Siege_Tank_Siege_Mode -> 154.3,
    Terran_Wraith -> 161.1,
    Terran_Battlecruiser -> 637.6,
    Protoss_Zealot -> 71.7,
    Protoss_Dragoon -> 178.2,
    Protoss_Archon -> 337.6,
    Protoss_Scout -> 338.2,
    Protoss_Corsair -> 173.7
  )
}
