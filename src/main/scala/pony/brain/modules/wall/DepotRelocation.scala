package pony
package brain
package modules
package wall

/** Native observations drive the opening; time alone cannot establish an expansion. */
private[pony] object DepotRelocation {
  sealed trait Step
  case object AwaitSaturation extends Step
  case object FinishTraining  extends Step
  case object Lift            extends Step
  case object Fly             extends Step
  case object Land            extends Step
  case object Established     extends Step
  def next(
      saturated: Boolean,
      training: Boolean,
      lifted: Boolean,
      nearDestination: Boolean,
      landedAtDestination: Boolean
  ): Step = {
    if (landedAtDestination) Established
    else if (!saturated && !lifted) AwaitSaturation
    else if (training && !lifted) FinishTraining
    else if (!lifted) Lift
    else if (!nearDestination) Fly
    else Land
  }
}
