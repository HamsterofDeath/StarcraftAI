package pony
package brain
package modules
package strategy

/** Game-time milestones in minutes that strategies use to stage production and research. */
final class GamePhase(minutes: Double) {
  def isBetween(from: Int, to: Int) = minutes >= from && minutes < to

  def isEarly = isBetween(0, 5)

  def isEarlyMid = isBetween(5, 9)

  def isMid = isBetween(9, 13)

  def isLateMid = isBetween(13, 20)

  def isLate = isBetween(20, 9999)

  def isSinceVeryEarlyMid = minutes >= 4

  def isSinceEarlyMid = minutes >= 5

  def isSinceAlmostMid = minutes >= 7

  def isSinceMid = minutes >= 8

  def isSincePostMid = minutes >= 9

  def isSinceLateMid = minutes >= 13

  def isSinceVeryLateMid = minutes >= 16

  def isBeforeLate = minutes <= 20

  def isAnyTime = true
}
