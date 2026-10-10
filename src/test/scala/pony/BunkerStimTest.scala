package pony

import org.specs2._
import org.specs2.matcher.MustMatchers
import pony.brain.modules.bunkers.BunkerStim._

class BunkerStimTest extends Specification with MustMatchers {

  def is =
    s2"""
       |Before the enemy is in reach every Marine whose stim ran out steps out at once $allAtOnce
       |While the bunker fights one steps out at a time, none while another is still out $oneAtATime
       |A stimmed or hurt Marine stays in $staysIn
       |A Marine on its way back stims first only while its bunker is threatened $onTheWay
       """.stripMargin

  private val fresh   = Inside(1, 40, 0)
  private val fresh2  = Inside(2, 40, 0)
  private val stimmed = Inside(3, 40, 20)
  private val hurt    = Inside(4, 20, 0)

  def allAtOnce = toUnload(Seq(fresh, fresh2, stimmed), fighting = false, someoneOutside = false) === Seq(1, 2)

  def oneAtATime = (toUnload(Seq(fresh, fresh2), fighting = true, someoneOutside = false) === Seq(1)) and
    (toUnload(Seq(fresh, fresh2), fighting = true, someoneOutside = true) === Nil)

  def staysIn = toUnload(Seq(stimmed, hurt), fighting = false, someoneOutside = false) === Nil

  def onTheWay = (stimsOnTheWay(threatened = true, stimTimer = 0, hitPoints = 40) must beTrue) and
    (stimsOnTheWay(threatened = false, stimTimer = 0, hitPoints = 40) must beFalse) and
    (stimsOnTheWay(threatened = true, stimTimer = 15, hitPoints = 40) must beFalse)
}
