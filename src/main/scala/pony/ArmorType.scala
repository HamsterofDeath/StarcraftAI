package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

sealed trait ArmorType {
  val tileSize: Size
  def transportSize: Int
  def damageFactorIfHitBy(damageType: DamageType): DamageFactor
}

case object Small extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Full
      case Explosive => Half
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 1
}

case object Medium extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Half
      case Explosive => ThreeQuarters
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 2
}

case object Indestructible extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = Zero

  override def transportSize = !!!("This should never happen")
}

case object Large extends ArmorType {
  override val tileSize = Size(2, 2)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Quarter
      case Explosive => Full
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 4
}

case object Building extends ArmorType {
  override val tileSize = Size(4, 3)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal => Full
      case Concussive => Quarter
      case Explosive => Full
      case _ => !!!(s"Check $damageType")
    }
  }

  override def transportSize = !!!("This should never happen")
}
