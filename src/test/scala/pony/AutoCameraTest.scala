package pony

import org.specs2._
import org.specs2.matcher.MustMatchers

class AutoCameraTest extends Specification with MustMatchers {

  def is =
    s2"""
       |The first candidate is shown at once $firstShown
       |A calm shot is kept for the minimum dwell time $dwellKept
       |A calm shot changes after the dwell time $dwellOver
       |A fight interrupts a calm shot immediately $fightInterrupts
       |A moving fight is followed without resetting the shot $fightFollowed
       |No candidates keep the current shot $noCandidates
       |The densest point counts its neighbours $densest
       |The screen centres a tile and clamps to the map $screenClamp
       |Far targets are jumped to, near ones are approached $panning
       """.stripMargin

  private def at(x: Int, y: Int) = MapTilePosition.shared(x, y)

  private val home  = CameraFocus(at(10, 10), CameraFocus.HomeScore, "home")
  private val army  = CameraFocus(at(40, 40), CameraFocus.ArmyScore + 6, "army")
  private val fight = CameraFocus(at(70, 20), CameraFocus.CombatScore + 30, "combat")

  private def director = new AutoCameraDirector(minDwellFrames = 96, interruptMargin = CameraFocus.EnemySightingScore)

  def firstShown = director.consider(0, Seq(home)) === Some(home)

  def dwellKept = {
    val d = director
    d.consider(0, Seq(home))
    d.consider(50, Seq(home, army)) === Some(home)
  }

  def dwellOver = {
    val d = director
    d.consider(0, Seq(home))
    d.consider(96, Seq(home, army)) === Some(army)
  }

  def fightInterrupts = {
    val d = director
    d.consider(0, Seq(army))
    d.consider(12, Seq(army, fight)) === Some(fight)
  }

  def fightFollowed = {
    val d = director
    d.consider(0, Seq(fight))
    val moved     = fight.copy(tile = at(75, 24))
    val followed  = d.consider(12, Seq(moved))
    val calmLater = d.consider(60, Seq(army))
    (followed === Some(moved)) and (calmLater === Some(moved))
  }

  def noCandidates = {
    val d = director
    d.consider(0, Seq(army))
    d.consider(500, Nil) === Some(army)
  }

  def densest = {
    val points = Seq(at(0, 0), at(30, 30), at(31, 30), at(30, 32), at(60, 60))
    AutoCameraDirector.densest(points, 4) === Some(at(30, 30) -> 3)
  }

  def screenClamp = {
    val centred   = CameraPan.screenFor(at(64, 64), 128, 128)
    val corner    = CameraPan.screenFor(at(0, 0), 128, 128)
    val farCorner = CameraPan.screenFor(at(127, 127), 128, 128)
    (centred === ((64 * 32 + 16 - 320) & ~7, (64 * 32 + 16 - 186) & ~7)) and
      (corner === (0, 0)) and
      (farCorner === ((128 * 32 - 640) & ~7, (128 * 32 - 372) & ~7))
  }

  def panning = {
    val jump     = CameraPan.step((0, 0), (3000, 0))
    val approach = CameraPan.step((0, 0), (400, 100))
    val finish   = CameraPan.step((100, 100), (104, 96))
    (jump === (3000, 0)) and (approach === (80, 20)) and (finish === (104, 96)) and
      (CameraPan.arrived((100, 100), (104, 96)) must beTrue)
  }
}
