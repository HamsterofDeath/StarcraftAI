package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class Upgrade(val nativeType: Either[UpgradeType, TechType]) {
  def mineralPriceForStep(step: Int) =
    nativeType.fold(_.mineralPrice(step), _.mineralPrice())

  def gasPriceForStep(step: Int) =
    nativeType.fold(_.gasPrice(step), _.gasPrice())

  def this(u: UpgradeType) = {
    this(Left(u))
  }

  def this(t: TechType) = {
    this(Right(t))
  }

  def energyCost = nativeType.fold(
    _ => throw new UnsupportedOperationException(s"Called on $this"),
    _.energyCost()
  )

  override def toString = s"Upgrade: ${nativeType.fold(_.toString, _.toString)}"

  override def equals(other: Any): Boolean = other match {
    case that: Upgrade =>
      nativeType == that.nativeType
    case _ => false
  }

  override def hashCode(): Int = {
    nativeType.hashCode()
  }
}
