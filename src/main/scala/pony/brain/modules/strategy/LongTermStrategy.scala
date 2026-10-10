package pony
package brain
package modules
package strategy

import pony.brain.modules.production.{IdealProducerCount, IdealUnitRatio}
import pony.terrain.ResourceArea
import pony.units.{Building, Mobile, UnitFactory}

import scala.reflect.ClassTag

/**
  * A long-term plan: what to produce, research and expand to. The `runs`/`uses` flags tell the always-on modules which
  * Terran campaign machinery this strategy wants, so a plugin opts in without extending a concrete strategy.
  */
trait LongTermStrategy extends HasUniverse {
  def name: String

  def phase: GamePhase = new GamePhase(time.minutes)

  def hasZeroOf[T <: Building: ClassTag] = ownUnits.allByType[T].isEmpty

  def buildAntiCloakNow: Boolean
  def suggestNextExpansion: Option[ResourceArea]
  def suggestUpgrades: Seq[UpgradeToResearch]
  def suggestUnits: Seq[IdealUnitRatio[? <: Mobile]]
  def suggestProducers: Seq[IdealProducerCount[? <: UnitFactory]]
  def suggestAddons: Seq[AddonToAdd] = Nil
  def determineScore: Int

  /** Expansion, mining, army and home-defence handling of the default Terran campaign. */
  def runsTerranCampaign: Boolean = false

  /** Four-Marine bunkers covering every active mineral field. */
  def usesBunkerDefense: Boolean = false

  /** Supply depots sealing the main choke. */
  def usesWallDefense: Boolean = false

  /** Massing the army and attacking the enemy base. */
  def usesCampaignLaunch: Boolean = false

  /** Flying factories, tank posts with depot boxes and mine screens spread over the map. */
  def usesCarpet: Boolean = false

  /** The wall never opens: dropships carry workers across it, and only air units leave the main. */
  def sealsMain: Boolean = false

  /** Battlecruisers raid the enemy and fly home to a repair crew (CruiserRaids). */
  def raidsWithCruisers: Boolean = false

  /** No more workers are trained beyond this many: the rest of the supply belongs to the army. */
  def maxWorkers: Int = 75

  /**
    * A gas-hungry army: a mineral bank buys another field (and its geyser) although the held ones are not fully
    * staffed, up to this many fields beyond the configured maximum.
    */
  def extraFieldsForGas: Int = 0
}
