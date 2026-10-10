package pony
package render

import pony.units.{Building, CanDie, Mobile}

import java.text.DecimalFormat

import pony.brain.modules.production.{EnqueueArmy, EnqueueFactories}
import pony.brain.modules.economy.GatherMineralsAtSinglePatch
import pony.brain.{HasUniverse, Universe}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class AiDebugRenderer(override val universe: Universe) extends AIPlugIn with HasUniverse {
  override val lazyWorld    = universe.world
  private val df            = new DecimalFormat("#0.00")
  private var lastTickNanos = System.nanoTime()

  class UnitStats() {
    private var detected = 0
    private var dead     = 0

    def format = {
      s"$alive/$detected/$dead"
    }

    def incDetected() = {
      detected += 1
    }

    def incDead() = {
      dead += 1
    }

    def alive = detected - dead
  }

  private val trackedUnits = mutable.HashMap.empty[SCUnitType, UnitStats]

  private val lastKnownPlanPriorities = mutable.HashMap.empty[SCUnitType, Double]

  universe.ownUnits.registerAdd_! { wu =>
    trackedUnits.getOrElseUpdate(wu.getClass, new UnitStats).incDetected()
  }
  universe.ownUnits.registerKill_! { wu =>
    trackedUnits(wu.getClass).incDead()
  }

  override protected def tickPlugIn(): Unit = {
    lazyWorld.debugger.debugRender { renderer =>
      val current = System.nanoTime()
      val diff    = current - lastTickNanos
      lastTickNanos = current

      val debugString = ArrayBuffer.empty[String]

      val speedFactor = {
        val secondsPerTick = diff.toDouble / 1000 / 1000 / 1000
        1 / secondsPerTick / 24
      }

      debugString += {
        val time     = universe.time.formatted
        val category = universe.time.categoryName

        val factor          = df.format(speedFactor)
        val currentStrategy = universe.strategy.current
        s"$category: $time (*${factor}), ${currentStrategy.name} (${currentStrategy
            .determineScore})"
      }

      debugString += {
        val locked      = resources.lockedResources
        val forceLocked = resources.forceLocks
        val locks       = resources.detailedLocks
        val allLocks    = forceLocked ++ locks
        val counts      = allLocks.map(_.whatFor).groupBy(identity).map { case (c, am) => c -> am.size }
        val details     = counts.toList.map { case (k, v) => s"${k.className}*$v" }.mkString(", ")
        s"Plan: ${locked.minerals}m, ${locked.gas}g, ${locked.supply}s, ${allLocks.size}L, $details"
      }

      debugString += {
        val locked = unitManager.requestedToBuild.groupBy(_.typeOfRequestedUnit)
          .map { case (k, v) =>
            s"${k.className}*${v.size}"
          }

        s"Planned (funded): ${locked.toList.sorted.mkString(", ")}"
      }

      debugString += {
        val locked = resources.failedToProvide.groupBy(_.whatFor).map { case (k, v) =>
          s"${k.className}*${v.size}"
        }

        s"In queue (no funds): ${locked.toList.sorted.mkString(", ")}"
      }

      debugString += {

        val missingUnits = unitManager.failedToProvideFlat.groupBy(_.typeOfRequestedUnit)
          .view.mapValues(_.size)
        val formatted = missingUnits
          .map { case (unitClass, howMany) => s"${unitClass.className}/$howMany" }
        s"Type/missing: ${formatted.toList.sorted.mkString(", ")}"
      }

      debugString ++= {
        universe.bases.allBases.flatMap { base =>
          base.myMineralGroup.map { mins =>
            val gatherJob = unitManager.allJobsByType[GatherMineralsAtSinglePatch]
              .filter(e => mins.contains(e.targetPatch))
            val stats            = resources.stats
            val minsGot          = stats.mineralsPerMinute.toInt
            val minsGotPerWorker = df.format(minsGot.toDouble / gatherJob.size)
            s"Base ${base.mainBuilding.unitIdText}: ${mins.value}m, ${
                gatherJob.size
              } workers, $minsGot income ($minsGotPerWorker avg)"
          }
        }.toList.sorted
      }

      val enqueueArmy        = universe.pluginByType[EnqueueArmy]
      val enqueueFactories   = universe.pluginByType[EnqueueFactories]
      val plan               = enqueueArmy.plan.buildThese.toMap
      val ratiosForUnits     = enqueueArmy.percentages
      val ratiosForProducers = enqueueFactories.ratios

      debugString ++= {
        trackedUnits.keysIterator
          .filter(c => classOf[CanDie] >= c)
          .filter(c => classOf[Mobile] >= c)
          .toList
          .sortBy(c => trackedUnits(c).alive)
          .takeRight(10)
          .sortBy(_.className).map { c =>
            val casted   = c.asInstanceOf[Class[? <: Mobile]]
            val priority = {
              val value = plan.get(casted) match {
                case op @ Some(newValue) =>
                  lastKnownPlanPriorities.put(casted, newValue)
                  op
                case None =>
                  lastKnownPlanPriorities.get(casted)
              }
              value.map { _.format }.getOrElse("x")
            }
            val wanted   = ratiosForUnits.wanted.get(casted).map(_.format).getOrElse("0")
            val existing = ratiosForUnits.existing.get(casted).map(_.format).getOrElse("0")
            s"${c.className.padTo(15, ' ')}: ${trackedUnits(c).format} $priority ($existing/$wanted)"
          }
      }

      debugString ++= {
        trackedUnits.keysIterator
          .filter(c => classOf[CanDie] >= c)
          .filter(c => classOf[Building] >= c)
          .filter(c => ratiosForProducers.find(_._1.typeOfFactory == c).isDefined)
          .toList
          .sortBy(c => trackedUnits(c).alive)
          .takeRight(10)
          .sortBy(_.className).map { c =>
            val casted   = c.asInstanceOf[Class[? <: Building]]
            val info     = ratiosForProducers.find(_._1.typeOfFactory == casted)
            val wanted   = info.map(_._1.format).getOrElse("?")
            val existing = info.map(_._2).getOrElse(0).toString
            s"${c.className.padTo(15, ' ')}: ${trackedUnits(c).format} ($existing/$wanted)"
          }
      }

      if (debugger.isFullDebug) {

        debugString ++= {
          universe.worldDominationPlan.allAttacks.map { att =>
            val id = att.uniqueId.toSome.map { id =>
              s"Attack $id with"
            }
            val force = att.force.size.toSome.map { i =>
              s" $i units"
            }
            val where = att.destination.where.toSome.map { tp =>
              s" attacking $tp"
            }
            val state = att.meetingStats.map { case (done, total) =>
              s", ${total - done} tbd"
            }

            (force :: where :: state :: Nil).flatten.mkString
          }
        }

        debugString ++= {
          val formatted = unitManager.jobsByType.map { case (jobType, members) =>
            s"${jobType.className}/${members.size}"
          }
          "Job/units:" :: formatted.toList.sorted
        }

        debugString ++= {
          unitManager.employers.map { emp =>
            s"$emp has ${unitManager.jobsOf(emp).size} units"
          }.toList.sorted
        }
      }

      debugString.foreach { renderer.drawTextOnScreen }
    }
  }
}
