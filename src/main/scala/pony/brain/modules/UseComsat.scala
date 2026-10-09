package pony
package brain
package modules

import pony.Upgrades.Terran._

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

class UseComsat(universe: Universe) extends DefaultBehaviour[Comsat](universe) {
  private val detectThese = ArrayBuffer.empty[Group[CanHide]]
  private val helpThese   = ArrayBuffer.empty[Group[CanDie]]

  override def onTick_!() = {
    super.onTick_!()
    detectThese ++= {
      mapNth(Primes.prime31, Seq.empty[Group[CanHide]]) {
        val dangerous = {
          enemies.allByType[CanHide]
            .iterator
            .filterNot(_.isExposed)
            .filter { cloaked =>
              ownUnits.allCompletedMobiles
                .exists(_.centerTile.distanceToIsLess(cloaked.centerTile, 8))
            }
            .toVector
        }

        val groups = GroupingHelper.groupTheseNow(dangerous, universe)
        groups.sortBy(-_.size)
      }
    }

    helpThese ++= {
      val attacked = {
        ownUnits.allCanDie
          .iterator
          .filterNot { u =>
            helpThese.exists(_.covers(u))
          }
          .filter(_.underAttackByCloaked)
      }

      val groups = GroupingHelper.groupTheseNow(attacked, universe)
      groups.sortBy(-_.size)
    }

    debug(
      s"Currently known cloaked groups: ${
          detectThese.map(e => s"${e.size}@${e.center}").mkString(", ")
        })",
      detectThese.nonEmpty
    )
  }

  override protected def wrapBase(comsat: Comsat) = new SingleUnitBehaviour[Comsat](comsat, meta) {
    override def describeShort = "Scan"

    override def toOrder(what: Objective) = {
      if (detectThese.nonEmpty && comsat.canCastNow(ScannerSweep)) {
        val first = detectThese.remove(0)
        Orders.ScanWithComsat(comsat, first.center).toList
      } else if (helpThese.nonEmpty && comsat.canCastNow(ScannerSweep)) {
        val first     = helpThese.remove(0)
        val surviving = first.survivingMembers
        if (surviving.nonEmpty && surviving.forall(_.underAttackByCloaked)) {
          Orders.ScanWithComsat(comsat, first.center).toList
        } else {
          Nil
        }
      } else {
        Nil
      }
    }
  }
}
