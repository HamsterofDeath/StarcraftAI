package pony
package brain

trait PreHiringResult[T <: WrapsUnit] {
  def hasAnyMissingRequirements = notExistingMissingRequiments.nonEmpty ||
    inProgressMissingRequirements.nonEmpty ||
    plannedMissingRequirements.nonEmpty ||
    jobbedMissingRequirements.nonEmpty

  def notExistingMissingRequiments: Set[Class[? <: Building]]  = Set.empty
  def inProgressMissingRequirements: Set[Class[? <: Building]] = Set.empty
  def plannedMissingRequirements: Set[Class[? <: Building]]    = Set.empty
  def jobbedMissingRequirements: Set[Class[? <: Building]]     = Set.empty
  def success: Boolean
  def canHire: CanHireInfo[T]
  def units = canHire.details

  override def toString = s"HiringResult($success, $canHire)"

  def mapOne[X](todo: T => X) = {
    var result = Option.empty[X]
    ifOne { in =>
      val ret = todo(in)
      result = Some(ret)
    }
    result
  }

  def ifOne[X](todo: T => X) = this match {
    case one: ExactlyOneSuccess[T] => todo(one.onlyOne)
    case _                         =>
  }

  def ifNotZero[X](todo: Seq[T] => X): Unit = ifNotZero(todo, {})

  def ifNotZero[X](todo: Seq[T] => X, orElse: X) = this match {
    case many: AtLeastOneSuccess[T] =>
      val canHireThese = many.canHire.details.toList
      todo(canHireThese)
    case _ => orElse
  }
}
