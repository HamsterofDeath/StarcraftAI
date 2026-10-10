package pony
package brain
package modules
package micro

import pony.combat.Spells

/** Medics with Optical Flare blind every enemy in reach worth it, detectors and costly fighters first. */
class BlindEnemies(universe: Universe) extends OneTimeUnitSpellCast(universe, Spells.Blind)
