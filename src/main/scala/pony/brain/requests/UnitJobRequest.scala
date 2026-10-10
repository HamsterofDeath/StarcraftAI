package pony
package brain
package requests

import pony.brain.budget.ResourceApprovalSuccess
import pony.brain.jobs.{Employer, Interruptable, UnitWithJob}
import pony.tech.Upgrade
import pony.terrain.ResourceArea
import pony.units.{Addon, CanBuildAddons, Mobile, UnitFactory, Upgrader, WorkerUnit, WrapsUnit}

import pony.brain.modules.production.AlternativeBuildingSpot

import scala.reflect.ClassTag

case class UnitJobRequest[T <: WrapsUnit: ClassTag](
    request: UnitRequest[T],
    employer: Employer[T],
    priority: Priority,
    makeSureDependenciesCleared: Set[Class[? <: WrapsUnit]] =
      Set.empty
) {

  def withRequest(f: UnitRequest[T] => UnitRequest[T]) = {
    copy(request = f(request))
  }

  assert(
    requestedUnitType >= moreSpecificType,
    s"$moreSpecificType > $requestedUnitType"
  )

  def canInterrupt(uwj: UnitWithJob[? <: WrapsUnit]) = {
    assert(priority > uwj.priority, "Oops :(")
    // perfect solution: match every type against every type and use heavy logic to determine the
    // result
    // reality: lazily add cases
    uwj match {
      case i: Interruptable[?] if i.interruptableNow => true
      case _                                         => false
    }
  }

  def allRequiredTypes = request.typeOfRequestedUnit.toSet ++ makeSureDependenciesCleared

  def priorityRule: Option[RateCandidate[T]] = {
    val picker = request.ratingFuntion
    picker.map { nat =>
      new RateCandidate[T] {
        def giveRating(forThatOne: UnitWithJob[T]): PriorityChain = nat(forThatOne)
      }
    }
  }

  def withOnlyAccepting(these: T => Boolean) = {
    copy(request = request.withFilter_!(these))
  }

  def clearable = request.clearable

  def wantsUnit(existingUnit: WrapsUnit) = moreSpecificType.isInstance(existingUnit) &&
    request.acceptableUntyped(existingUnit)

  def moreSpecificType = request.typeOfRequestedUnit

  def requestedUnitType = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[? <: T]]

  def onClear(): Unit = request.dispose()
}

object UnitJobRequest {
  def upgraderFor(
      upgrade: Upgrade,
      employer: Employer[Upgrader],
      priority: Priority = Priority.Upgrades
  ) = {
    val actualClass = employer.race.techTree.upgraderFor(upgrade).asInstanceOf[Class[Upgrader]]
    val req         = AnyUnitRequest(actualClass, 1)
    UnitJobRequest(req, employer, priority).withOnlyAccepting(!_.isDoingResearch)
  }

  def builderOf[T <: Mobile, F <: UnitFactory: ClassTag](
      wantedType: Class[? <: T],
      employer: Employer[F],
      priority: Priority = Priority
        .Default
  ): UnitJobRequest[F] = {

    val actualClass = employer.universe.forces.myRace.specialize(implicitly[ClassTag[F]].runtimeClass
      .asInstanceOf[Class[F]])
    val req = AnyFactoryRequest[F, T](actualClass, 1, wantedType)

    UnitJobRequest(req, employer, priority)
  }

  def constructor[T <: WorkerUnit: ClassTag](
      employer: Employer[T],
      priority: Priority = Priority
        .ConstructBuilding
  ): UnitJobRequest[T] = {

    val actualClass = employer.universe.forces.myRace.specialize(implicitly[ClassTag[T]].runtimeClass)
      .asInstanceOf[Class[T]]
    val req = AnyUnitRequest(actualClass, 1)
      .withCherryPicker_!(WorkerUnit.currentPriority)

    UnitJobRequest(req, employer, priority)
  }

  def addonConstructor[T <: CanBuildAddons: ClassTag](
      employer: Employer[T],
      what: Class[? <: Addon],
      priority: Priority = Priority
        .ConstructBuilding
  ) = {

    val actualClass = employer.universe.forces.myRace.specialize(what)
    val mainType    = employer.race.techTree.mainBuildingOf(what).asInstanceOf[Class[T]]
    val req         = AnyUnitRequest(mainType, 1)
      .withFilter_!(e => !e.isBeingCreated && !e.hasAddonAttached && !e.isBuildingAddon)

    UnitJobRequest(req, employer, priority, Set(what))
  }

  def idleOfType[T <: WrapsUnit: ClassTag](
      employer: Employer[T],
      ofType: Class[? <: T],
      amount: Int = 1,
      priority: Priority = Priority.Default
  ) = {
    val realType               = employer.universe.forces.myRace.specialize(ofType)
    val req: AnyUnitRequest[T] = AnyUnitRequest(realType, amount)

    UnitJobRequest(req, employer, priority)
  }

  def newOfType[T <: WrapsUnit: ClassTag](
      universe: Universe,
      employer: Employer[T],
      ofType: Class[? <: T],
      funding: ResourceApprovalSuccess,
      amount: Int = 1,
      priority: Priority = Priority.Default,
      customBuildingPosition: AlternativeBuildingSpot =
        AlternativeBuildingSpot
          .useDefault,
      belongsTo: Option[ResourceArea] = None
  ) = {
    val actualType               = universe.forces.myRace.specialize(ofType)
    val req: BuildUnitRequest[T] = {
      BuildUnitRequest(
        universe,
        actualType,
        amount,
        funding,
        priority,
        customBuildingPosition,
        belongsTo
      )
    }
    // this one needs to survive across ticks
    req.persistant_!()
    UnitJobRequest[T](req, employer, priority)
  }
}
