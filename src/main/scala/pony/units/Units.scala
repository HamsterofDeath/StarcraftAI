package pony
package units

import pony.combat.{
  AirWeapon, ArmedBuildingCoveringAir, ArmedBuildingCoveringGround, ArmedBuildingCoveringGroundAndAir, ArmedMobile,
  GroundWeapon
}
import pony.geometry.MapTilePosition

import bwapi.Game
import pony.brain.{HasUniverse, Universe}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class Units(game: Game, hostile: Boolean, override val universe: Universe) extends HasUniverse {
  private val killListeners      = mutable.HashMap.empty[Int, OnKillListener[?]]
  private val killListenersOnAll = mutable.ArrayBuffer.empty[WrapsUnit => Unit]
  private val newUnitListeners   = mutable.ArrayBuffer.empty[WrapsUnit => Unit]
  private val fresh              = ArrayBuffer.empty[WrapsUnit]
  private val graveyard          = mutable.HashMap.empty[Int, WrapsUnit]
  private val classIndexes       = mutable.HashSet.empty[Class[?]]
  private val preparedByClass    = multiMap[Class[?], WrapsUnit]
  private val nativeIdToUnit     = mutable.HashMap.empty[Int, WrapsUnit]

  private var initial = true

  def byNative(nativeUnit: bwapi.Unit) = if (nativeUnit == null) None else byId(nativeUnit.getID)

  def byId(nativeId: Int) = nativeIdToUnit.get(nativeId)

  def byIdExpectExisting(nativeId: Int) = nativeIdToUnit(nativeId)

  def consumeFresh_![X](f: WrapsUnit => X) = {
    fresh.foreach(f)
    fresh.clear()
  }

  def registerKill_![T <: WrapsUnit](listener: OnKillListener[T]): Unit = {
    // this will always replace the latest listener
    killListeners += ((listener.nativeUnitId, listener))
  }

  def registerKill_!(listener: WrapsUnit => Unit): Unit = {
    killListenersOnAll += listener
  }

  def registerAdd_!(listener: WrapsUnit => Unit): Unit = {
    newUnitListeners += listener
  }

  def dead_!(dead: Seq[bwapi.Unit]) = {
    dead.foreach { u =>
      nativeIdToUnit.get(u.getID).foreach { died =>
        died match {
          case cd: CanDie =>
            cd.notifyDead_!()
          case _ =>
        }
        killListeners.get(u.getID).foreach { e =>
          e.onKillUnTyped(died)
          killListeners.remove(u.getID)
        }
        killListenersOnAll.foreach { _.apply(died) }
        val nowDead = removeUnit(u)
        nowDead.foreach {
          case cd: CanDie => cd.notifyDead_!()
          case _          =>
        }
        nowDead.foreach { e =>
          graveyard += ((u.getID, e))
        }
      }
    }
  }

  /**
    * A unit of ours the enemy took over (Mind Control) is lost to us, the hostile side picks it up as theirs: in game 1
    * on 3c1dae4 two controlled cruisers stayed in our fleet for the rest of the game, counted as hurt, shooting at ours.
    * A geyser the enemy builds on changes sides as well, but it is neutral ground that stays known as it always was.
    */
  def renegade_!(u: bwapi.Unit): Unit = if (ownAndNeutral && taken(u.getID)) dead_!(Seq(renegade("event")(u)))

  private def taken(id: Int) =
    nativeIdToUnit.get(id).exists(w => !w.isInstanceOf[Resource] && forces.isEnemy(w.nativeUnit))

  /**
    * An enemy unit seen in the hands of no enemy has left the enemy: a Terran add-on whose building lifted off belongs to
    * nobody until a building lands at it again. In game 1 vs Terran on e73348a such a machine shop held nine sieged tanks
    * in the razed enemy main for forty game minutes; no unit attacks it, and the game ends without it. Out of sight, a
    * unit shows no owner at all, so only a visible one counts.
    */
  private def defected(w: WrapsUnit) = w.nativeUnit.isVisible && !forces.isEnemy(w.nativeUnit)

  private def renegade(by: String)(u: bwapi.Unit): bwapi.Unit = {
    NativeMatchEvidence.trace("unit-renegade", s"unit=${u.getID} type=${u.getType} by=$by")
    u
  }

  private def removeUnit(u: bwapi.Unit) = {
    val removed = nativeIdToUnit.remove(u.getID)
    removed.foreach { what =>
      classIndexes.foreach { c =>
        preparedByClass.removeBinding(c, what)
      }
    }
    removed.foreach(_.notifyRemoved_!())
    removed
  }

  def buildingAt(upperLeft: MapTilePosition) = {
    allBuildings.find(_.area.upperLeft == upperLeft)
  }

  def allBuildings = allByType[Building]

  def allBuildingsWithWeapons = allByType[ArmedBuildingCoveringGroundAndAir]

  def allBuildingsWithGroundWeapons = allByType[ArmedBuildingCoveringGround]

  def allBuildingsWithAirWeapons = allByType[ArmedBuildingCoveringAir]

  def allDetectors = allByType[Detector]

  def allCompletedMobiles = allMobiles.filterNot(_.isBeingCreated)

  def allWithGroundWeapon = allByType[GroundWeapon]

  def allMobilesWithWeapons = allByType[ArmedMobile]

  def allWithAirWeapon = allByType[AirWeapon]

  def allMobiles = allByType[Mobile]

  def allAddonBuilders = allByType[CanBuildAddons]

  def allAddons = allByType[Addon]

  def existsIncomplete(c: Class[? <: WrapsUnit]) = allByClass(c).exists(_.isBeingCreated)

  def existsComplete(c: Class[? <: WrapsUnit]) = allByClass(c).exists(!_.isBeingCreated)

  def ownsByType(c: Class[? <: WrapsUnit]) = {
    nativeIdToUnit.values.exists(c.isInstance)
  }

  def geysirs = allByType[Geysir]

  def allByType[T <: WrapsUnit: ClassTag] = {
    val lookFor = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]
    allByClass(lookFor)
  }

  def allByClass[T <: WrapsUnit](lookFor: Class[T]) = {
    def lazyCreate = {
      classIndexes += lookFor
      mutable.HashSet.empty ++= allKnownUnits.filter { e => lookFor.isInstance(e) }
    }
    val cached = preparedByClass.getOrElseUpdate(lookFor, lazyCreate)
    cached.asInstanceOf[collection.Set[T]]
  }

  def allKnownUnits = nativeIdToUnit.valuesIterator

  def allRelevant = nativeIdToUnit.valuesIterator.filterNot(_.isInstanceOf[Irrelevant])

  def allCanDie = allByType[CanDie]

  import scala.jdk.CollectionConverters._

  def firstByType[T: ClassTag]: Option[T] = {
    val lookFor = implicitly[ClassTag[T]].runtimeClass
    inFaction.find(lookFor.isInstance).map(_.asInstanceOf[T])
  }

  def inFaction = allKnownUnits.filter(_.nativeUnit.getPlayer == game.self())

  def minerals = allByType[MineralPatch]

  def allMobilesAndBuildings = allCompletedMobiles ++ allBuildings

  def tick(): Unit = {
    if (initial) {
      initial = false
      init()
    }
    // sometimes units die without the event being triggered
    if (universe.currentTick % 19 == 0) {
      val dead = allRelevant.filterNot(_.isInGame).map { e =>
        warn(s"Unit $e died without event")
        e.nativeUnit
      }.toSeq
      dead_!(dead)
      if (ownAndNeutral)
        dead_!(allRelevant.map(_.nativeUnit).filter(u => taken(u.getID)).map(renegade("sweep")).toSeq)
      else {
        // seen again, it is the enemy's again
        val gone = allRelevant.filter(defected).map(w => renegade("sweep")(w.nativeUnit)).toSeq
        dead_!(gone)
        gone.foreach(u => graveyard -= u.getID)
      }
    }

    val addThese = {
      if (ownAndNeutral)
        game.self().getUnits.asScala
      else
        game.enemies().asScala.flatMap(_.getUnits.asScala)
    }
    addThese.foreach { addUnit }
  }

  def registerUnit(u: bwapi.Unit, lifted: WrapsUnit) = {
    newUnitListeners.foreach { _.apply(lifted) }
    nativeIdToUnit.put(u.getID, lifted)
    classIndexes.foreach { c =>
      if (c.isInstance(lifted)) {
        preparedByClass.addBinding(c, lifted)
      }
    }
  }

  private def init(): Unit = {
    if (ownAndNeutral) {
      game.getMinerals.asScala.foreach { addUnit }
      game.getGeysers.asScala.foreach { addUnit }
    }
  }

  private def addUnit(u: bwapi.Unit): Unit = {
    val record = {
      if (ownAndNeutral) {
        forces.isNotEnemy(u)
      } else {
        forces.isEnemy(u)
      }
    }
    if (record) {
      if (!graveyard.contains(u.getID)) {
        nativeIdToUnit.get(u.getID) match {
          case None =>
            val lifted = UnitWrapper.lift(u)
            fresh += lifted
            info(s"${ownAndNeutral.ifElse("Own", "Hostile")} unit added: $lifted")
            registerUnit(u, lifted)
          case Some(unit) if unit.initialNativeType != u.getType =>
            if (unit.shouldReRegisterOnMorph) {
              info(s"Unit morphed from ${unit.initialNativeType} to ${u.getType}")
              val lifted = UnitWrapper.lift(u)

              // clean up old indexes
              classIndexes.foreach { c =>
                if (c.isInstance(unit)) {
                  preparedByClass.removeBinding(c, unit)
                }
              }

              registerUnit(u, lifted)
              fresh += lifted
              unit.onMorph(u.getType)
            }
          case _ => // noop
        }
      } else {
        warn("zombie?")
      }
    }
  }

  private def ownAndNeutral = !hostile

}
