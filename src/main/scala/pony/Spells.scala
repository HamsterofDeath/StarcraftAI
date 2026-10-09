package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

object Spells {

  case object Lockdown extends SingleTargetSpell[Ghost, Mechanic](Upgrades.Terran.GhostStop) {
    override def isAffected(m: Mechanic) = {
      m.isLocked
    }
  }

  case object WraithCloak extends SingleTargetSpell[Wraith, Wraith](Upgrades.Terran.WraithCloak) {
    override def isAffected(m: Wraith) = {
      m.isCloaked
    }
  }

  case object TankSiege extends SingleTargetSpell[Tank, Tank](Upgrades.Terran.TankSiegeMode) {
    override def isAffected(m: Tank) = {
      m.isSieged
    }
  }

  case object Stimpack
      extends SingleTargetSpell[CanUseStimpack, CanUseStimpack](Upgrades.Terran.InfantryCooldown) {
    override def isAffected(m: CanUseStimpack) = {
      m.isStimmed
    }
  }

  case object Irradiate
      extends SingleTargetSpell[ScienceVessel, Organic](Upgrades.Terran.Irradiate) {
    override def isAffected(m: Organic) = {
      m.isIrradiated
    }
  }

  case object DefenseMatrix
      extends SingleTargetSpell[ScienceVessel, Mobile](Upgrades.Terran.Defensematrix) {
    override def isAffected(m: Mobile) = m.matrixHp >= 25 // allow refreshing the matrix

    override def castOn = OwnUnits

    override def shouldActivateOn(validated: Mobile) = {
      validated.isBeingAttacked
    }
  }

  case object Blind extends SingleTargetSpell[Medic, Organic](Upgrades.Terran.MedicFlare) {
    override def isAffected(m: Organic) = m.isBlinded
  }

}
