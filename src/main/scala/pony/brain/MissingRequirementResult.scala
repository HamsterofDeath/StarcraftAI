package pony
package brain

class MissingRequirementResult[T <: WrapsUnit](needs: Set[Class[_ <: Building]],
                                               incomplete: Set[Class[_ <: Building]],
                                               planned: Set[Class[_ <: Building]],
                                               jobbed: Set[Class[_ <: Building]])
  extends PreHiringResult[T] {
  def success = false

  def canHire = CanHireInfo.empty

  override def notExistingMissingRequiments = needs

  override def inProgressMissingRequirements = incomplete

  override def plannedMissingRequirements = planned

  override def jobbedMissingRequirements = jobbed
}
