package pony
package brain

import scala.collection.mutable.ArrayBuffer

object UnitRequest {

  private var counter = 0

  def nextId() = {
    counter += 1
    counter
  }

  object CherryPickers {
    def cherryPickWorkerByDistance[W <: WorkerUnit](target: MapTilePosition)(
        distanceEvaluation: MapTilePosition => Double = _
          .distanceSquaredTo(
            target
          )
    ) =
      (job: UnitWithJob[W]) => {
        val u       = job.universe
        val altUnit = job.unit
        val walkMap = u.mapLayers.rawWalkableMap

        val sameArea         = walkMap.areInSameWalkableArea(altUnit.currentTile, target)
        val canSee           = walkMap.connectedByLine(altUnit.currentTile, target)
        val distance         = distanceEvaluation(altUnit.currentTile)
        val busyness         = WorkerUnit.currentPriority(job)
        val weightedDistance = distance * busyness.sum
        PriorityChain(sameArea.ifElse(0, 1), canSee.ifElse(0, 1), weightedDistance)
      }
  }
}

trait UnitRequest[T <: WrapsUnit] {

  private val id                       = UnitRequest.nextId()
  private val onDisposeActions         = ArrayBuffer.empty[OnClearAction]
  private var picker                   = Option.empty[UnitWithJob[T] => PriorityChain]
  private var filter                   = Option.empty[T => Boolean]
  private var autoCleanAfterTick       = true
  private var keepResourcesLocked      = false
  def withFilter_!(rule: T => Boolean) = {
    filter = Some(rule)
    this
  }
  def withCherryPicker_!(rate: UnitWithJob[T] => PriorityChain) = {
    picker = Some(rate)
    this
  }

  private var trainingSpot = Option.empty[MapTilePosition]

  /** A unit trained for this request comes from the producer nearest this tile, on its side of a sealed wall. */
  def trainNear_!(tile: MapTilePosition) = {
    trainingSpot = Some(tile)
    this
  }

  def trainNear     = trainingSpot
  def ratingFuntion = picker
  def priority: Priority
  def typeOfRequestedUnit: Class[? <: T]
  def amount: Int
  def acceptableUntyped(unit: WrapsUnit) = includesByType(unit) &&
    acceptable(unit.asInstanceOf[T])
  def includesByType(unit: WrapsUnit): Boolean = typeOfRequestedUnit.isInstance(unit)
  def acceptable(unit: T)                      = filter.map(_.apply(unit)).getOrElse(true)
  def clearable                                = autoCleanAfterTick
  def dispose(): Unit                          = {
    trace(s"$debugString is being disposed of")
    onDisposeActions.foreach(_.onClear())
  }

  def debugString = s"Req[$id]$this"

  if (classOf[Building] >= typeOfRequestedUnit) {
    keepResourcesLocked_!()
  }

  def doOnDispose_![X](u: => X) = {
    onDisposeActions += (() => u)
  }

  def persistant_!(): Unit = {
    autoCleanAfterTick = false
  }

  def clearableInNextTick_!(): Unit = {
    trace(s"$debugString will be cleared next tick")
    autoCleanAfterTick = true
  }

  def forceUnlockOnDispose_!(): Unit = {
    keepResourcesLocked = false
  }

  def keepResourcesLocked_!(): Unit = {
    keepResourcesLocked = true
  }

  def unlocksResourcesOnDispose = !keepResourcesLocked

  override def toString = s"UnitRequest($typeOfRequestedUnit, $amount)"
}
