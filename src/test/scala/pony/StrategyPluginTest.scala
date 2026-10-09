package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.strategy._

class StrategyPluginTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Every built-in strategy is discovered through the service file $discovered
       |The bot picks among the six classic strategies and the ramp wall on its own $autoSelectable
       |A configured key selects that strategy $configured
       |An unknown key stops the bot and names the known keys $unknownKey
       |Two plugins with one key are rejected $duplicateKeys
       |A key must be lowercase letters, digits and dashes $keyFormat
       |Strategies declare the campaign machinery they use $capabilities
       """.stripMargin

  private val universe: pony.brain.Universe = null

  private lazy val registry = StrategyRegistry.discover()

  def discovered = registry.keys === Vector(
    "air-control",
    "campaign",
    "carpet",
    "factory",
    "heavy-air",
    "heavy-metal",
    "idle",
    "infantry",
    "rampwall",
    "skywall",
    "tvp",
    "tvt",
    "tvz"
  )

  def autoSelectable =
    registry.plugins.filter(_.autoSelectable).map(_.key).toSet === Set(
      "campaign",
      "rampwall",
      "tvp",
      "air-control",
      "heavy-air",
      "tvt",
      "tvz"
    )

  def configured = {
    val selector = new StrategySelector(universe, registry, Some("heavy-metal"))
    selector.current.name === "Heavy metal"
  }

  def unknownKey = new StrategySelector(universe, registry, Some("nope")) must throwA[IllegalArgumentException](
    "Unknown strategy 'nope'; known: default, air-control"
  )

  def duplicateKeys = {
    val clash = Vector(new IdleAroundPlugin, new IdleAroundPlugin)
    new StrategyRegistry(clash) must throwA[IllegalStateException]
  }

  def keyFormat = new StrategyPlugin("Bad Key", "", false)(new IdleAround(_)) {} must throwA[IllegalArgumentException]

  def capabilities = {
    def strategy(key: String) = registry.find(key).get.create(universe)
    val campaign              = strategy("campaign")
    val carpet                = strategy("carpet")
    val skywall               = strategy("skywall")
    val rampwall              = strategy("rampwall")
    val heavyMetal            = strategy("heavy-metal")
    val idle                  = strategy("idle")
    (campaign.runsTerranCampaign && campaign.usesBunkerDefense && campaign.usesCampaignLaunch must beTrue) and
      (carpet.usesCarpet && carpet.usesWallDefense && !carpet.usesCampaignLaunch must beTrue) and
      (skywall.usesWallDefense && !skywall.usesBunkerDefense must beTrue) and
      (rampwall.runsTerranCampaign && rampwall.usesWallDefense && rampwall.usesBunkerDefense &&
        rampwall.usesCampaignLaunch must beTrue) and
      (heavyMetal.runsTerranCampaign && heavyMetal.usesBunkerDefense must beTrue) and
      (idle.runsTerranCampaign || idle.usesBunkerDefense || idle.usesCarpet must beFalse)
  }
}
