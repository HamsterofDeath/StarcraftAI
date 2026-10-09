package pony
package brain

class MissingRequirementResult[T <: WrapsUnit](
    needs: Set[Class[? <: Building]],
    incomplete: Set[Class[? <: Building]],
    planned: Set[Class[? <: Building]],
    jobbed: Set[Class[? <: Building]]
) extends PreHiringResult[T] {
  def success = false

  def canHire = CanHireInfo.empty

  override def notExistingMissingRequiments = needs

  override def inProgressMissingRequirements = incomplete

  override def plannedMissingRequirements = planned

  override def jobbedMissingRequirements = jobbed
}
