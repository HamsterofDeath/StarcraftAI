package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.KitingPolicy._

class KitingPolicyTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Without melee threats the unit is free for other behaviours $free
       |A shot under way is never interrupted $firing
       |A unit retreats as soon as its shot is released $firedThenRetreats
       |A reloading unit attacks a target in range shortly before the reload ends $leadAttack
       |A ready weapon focuses the weakest enemy in range $weakestInRange
       |A ready weapon attacks the closest enemy when none is in range $closestOutOfRange
       |A reloading unit retreats from a threat that would catch it $retreats
       |A reloading unit holds when every threat is far enough away $holds
       |A retreat never goes into unwalkable ground $avoidsWalls
       |A retreat prefers open ground over a corner $avoidsCorners
       |An enemy with equal or longer reach is fought instead of run from $noRetreatFromLongerReach
       |An enemy that cannot hit back is attacked and never run from $harmlessTarget
       |A shooter does not run from an enemy that is about as fast $noRetreatFromFasterEnemy
       |Focus fire skips an enemy whose committed damage already kills it $noOverkill
       |A unit too slow to kite steps back only when a melee enemy is in contact $stepBack
       |A reloading unit closes in on a target that cannot hit back $approachHarmless
       |A reloading unit does not close in on a melee enemy that would reach it $noApproachIntoDanger
       |The unit static defence shoots at leaves its reach after firing $leavesTurretReach
       |Units static defence ignores keep firing from inside its reach $staysWhenNotAimedAt
       |With Dance.All every reloading unit leaves the reach of static defence, with Dance.Off none $danceModes
       |A unit outside static defence returns just in time to fire on arrival $returnsInTime
       |Static defence the unit outranges is fought from outside its reach $outrangedTurret
       """.stripMargin

  private val open: Point => Boolean = p => p.x >= 0 && p.y >= 0 && p.x < 2048 && p.y < 2048

  /** A vulture-like shooter in the middle of an open map. */
  private def shooter(cooldown: Int = 0, firing: Boolean = false, at: Point = Point(1024, 1024)) =
    Shooter(at, range = 176, cooldown = cooldown, firing = firing, speed = 6.4)

  /** A zealot-like threat. */
  private def zealot(id: Int, x: Double, y: Double, durability: Int = 160) =
    Threat(id, Point(x, y), durability, reach = 36, speed = 4)

  def free = decide(shooter(), Nil, open) === Free

  def firing = decide(shooter(firing = true), Seq(zealot(1, 1100, 1024)), open) === Hold

  def firedThenRetreats = decide(shooter(cooldown = 28, firing = true), Seq(zealot(1, 1080, 1024)), open) match {
    case Retreat(_) => ok
    case other      => other === Retreat(Point(0, 0))
  }

  def leadAttack = {
    val near = Seq(zealot(1, 1150, 1024))
    (decide(shooter(cooldown = 3), Seq(zealot(1, 1600, 1024)), open) must not(beEqualTo(Shoot(1)))) and
      (decide(shooter(cooldown = 3), near, open) === Shoot(1)) and
      (decide(shooter(cooldown = 3), near, open, lead = 0) must not(beEqualTo(Shoot(1))))
  }

  def weakestInRange = {
    val threats = Seq(zealot(1, 1100, 1024, 160), zealot(2, 1150, 1024, 40), zealot(3, 1500, 1024, 10))
    decide(shooter(), threats, open) === Shoot(2)
  }

  def closestOutOfRange = decide(shooter(), Seq(zealot(1, 1400, 1024), zealot(2, 1300, 1024)), open) === Shoot(2)

  def retreats = {
    val me     = shooter(cooldown = 20)
    val threat = zealot(1, 1124, 1024)
    decide(me, Seq(threat), open) match {
      case Retreat(to) => to.distanceTo(threat.at) must be_>(me.at.distanceTo(threat.at))
      case other       => other === Retreat(Point(0, 0))
    }
  }

  def holds = decide(shooter(cooldown = 5), Seq(zealot(1, 1170, 1024)), open) === Hold

  def avoidsWalls = {
    val wallToTheLeft: Point => Boolean = p => open(p) && p.x > 1000
    val me                              = shooter(cooldown = 20, at = Point(1010, 1024))
    decide(me, Seq(zealot(1, 1080, 1024)), wallToTheLeft) match {
      case Retreat(to) => (wallToTheLeft(to) must beTrue) and (to.x must be_>(1000.0))
      case other       => other === Retreat(Point(0, 0))
    }
  }

  def noRetreatFromLongerReach = {
    val dragoonLike = Threat(1, Point(1100, 1024), 180, reach = 200, speed = 5)
    decide(shooter(cooldown = 20), Seq(dragoonLike), open) === Hold
  }

  def harmlessTarget = {
    val cannotHitBack = Threat(1, Point(1060, 1024), 160, reach = 0, speed = 4)
    (decide(shooter(), Seq(cannotHitBack), open) === Shoot(1)) and
      (decide(shooter(cooldown = 20), Seq(cannotHitBack), open) === Hold)
  }

  def noRetreatFromFasterEnemy = {
    val dragoonFast = Threat(1, Point(1100, 1024), 180, reach = 36, speed = 6.2)
    decide(shooter(cooldown = 20), Seq(dragoonFast), open) === Hold
  }

  def noOverkill = {
    val threats = Seq(zealot(1, 1100, 1024, 40), zealot(2, 1150, 1024, 120))
    (decide(shooter(), threats, open) === Shoot(1)) and
      (decide(shooter(), threats, open, committed = Map(1 -> 40.0)) === Shoot(2)) and
      (decide(shooter(), threats, open, committed = Map(1 -> 20.0)) === Shoot(1))
  }

  /** A goliath-like shooter: slower relative to zealots than kiting needs. */
  private def slowShooter(cooldown: Int) = Shooter(Point(1024, 1024), 176, cooldown, firing = false, speed = 4.57)

  def stepBack = {
    val inContact = zealot(1, 1060, 1024) // gap 0
    val nearby    = zealot(1, 1100, 1024) // gap 40
    (decide(slowShooter(15), Seq(inContact), open) match {
      case Retreat(to) => to.distanceTo(Point(1024, 1024)) must be_<(60.0)
      case other       => other === Retreat(Point(0, 0))
    }) and (decide(slowShooter(15), Seq(nearby), open) === Hold)
  }

  def approachHarmless = {
    val farAway = Threat(1, Point(1500, 1024), 160, reach = 0, speed = 4)
    decide(shooter(cooldown = 20), Seq(farAway), open) match {
      case Approach(to) => (to.x must be_>(1024.0)) and (Point(1500, 1024).distanceTo(to) must be_<(176.0))
      case other        => other === Approach(Point(0, 0))
    }
  }

  def noApproachIntoDanger = decide(slowShooter(25), Seq(zealot(1, 1300, 1024)), open) === Hold

  /** A wraith-like shooter facing a photon cannon 240 pixels to the east. */
  private val wraith = Shooter(Point(1024, 1024), 190, cooldown = 25, firing = false, speed = 6.67)
  private def cannon(aimsAtMe: Boolean, x: Double = 1264) =
    Threat(1, Point(x, 1024), 200, reach = 258, speed = 0, aimsAtMe = aimsAtMe)

  def leavesTurretReach = decide(wraith, Seq(cannon(aimsAtMe = true)), open) match {
    case Retreat(to) => to.x must be_<(1024.0)
    case other       => other === Retreat(Point(0, 0))
  }

  def staysWhenNotAimedAt = decide(wraith, Seq(cannon(aimsAtMe = false)), open) === Hold

  def danceModes =
    (decide(wraith, Seq(cannon(aimsAtMe = false)), open, dance = Dance.All) must beLike { case Retreat(_) => ok }) and
      (decide(wraith, Seq(cannon(aimsAtMe = true)), open, dance = Dance.Off) === Hold)

  def returnsInTime = {
    // 300 pixels away the wraith needs (300 - 190) / 6.67 = 16.5 frames to get in range
    val far = cannon(aimsAtMe = true, x = 1324)
    (decide(wraith.copy(cooldown = 20), Seq(far), open) === Shoot(1)) and
      (decide(wraith.copy(cooldown = 25), Seq(far), open) === Hold)
  }

  def outrangedTurret = {
    val siegedTank = Shooter(Point(1024, 1024), 400, cooldown = 20, firing = false, speed = 4)
    decide(siegedTank, Seq(cannon(aimsAtMe = false)), open) must beLike { case Retreat(to) => to.x must be_<(1024.0) }
  }

  def avoidsCorners = {
    // threat comes from the middle; straight away from it is the corner at (0, 0)
    val me = shooter(cooldown = 20, at = Point(120, 120))
    decide(me, Seq(zealot(1, 190, 190)), open) match {
      // straight away from the threat would end 73 px from the corner; open ground keeps clear of it
      case Retreat(to) => to.distanceTo(Point(0, 0)) must be_>(120.0)
      case other       => other === Retreat(Point(0, 0))
    }
  }
}
