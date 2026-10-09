package pony
package brain
package modules

import scala.compiletime.uninitialized

trait AlternativeBuildingSpot {
  def init_!(): Unit
  def isEmpty = !shouldUse

  def shouldUse: Boolean

  def evaluateCostly: Option[MapTilePosition]

  def predefined: Option[MapTilePosition]
  def requestedPosition: Option[MapTilePosition]                            = predefined
  def allowDefaultFallback: Boolean                                         = true
  def resolve(default: => Option[MapTilePosition]): Option[MapTilePosition] =
    predefined.orElse(evaluateCostly).orElse(if (allowDefaultFallback) default else None)
}

object AlternativeBuildingSpot {
  def fromValidatedPreset(position: MapTilePosition)(valid: => Boolean): AlternativeBuildingSpot =
    new AlternativeBuildingSpot {
      private var checked               = Option.empty[MapTilePosition]
      override def init_!(): Unit       = { checked = if (valid) Some(position) else None }
      override def shouldUse            = true
      override def allowDefaultFallback = false
      override def requestedPosition    = Some(position)
      override def predefined           = checked
      override def evaluateCostly       = None
    }
  val useDefault = new AlternativeBuildingSpot {
    override def evaluateCostly = None

    override def shouldUse = false

    override def predefined = None

    override def init_!(): Unit = {}
  }

  def fromExpensive[X](initOnMainThread: => X)(evalPosition: (X) => Option[MapTilePosition]): AlternativeBuildingSpot =
    new AlternativeBuildingSpot {
      private var initPackage: X = uninitialized

      override def init_!(): Unit = {
        initPackage = initOnMainThread
      }

      override def shouldUse = true

      override def evaluateCostly = evalPosition(initPackage)

      override def predefined = None
    }

  def fromPreset(fixedPosition: MapTilePosition): AlternativeBuildingSpot = fromPreset(
    Some(fixedPosition)
  )

  def fromPreset(fixedPosition: Option[MapTilePosition]): AlternativeBuildingSpot = new AlternativeBuildingSpot {

    fixedPosition.foreach { where =>
      assert(where.x < 1000)
      assert(where.y < 1000)
    }

    override def shouldUse = true

    override def evaluateCostly = throw new RuntimeException("This should not be called")

    override def predefined = fixedPosition

    override def init_!(): Unit = {}
  }
}
