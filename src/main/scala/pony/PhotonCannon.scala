package pony

import bwapi.{Unit => APIUnit, _}
import pony.Upgrades.Terran.{Nuke, ScannerSweep}
import pony.Upgrades.{IsTech, SinglePointMagicSpell, SingleTargetMagicSpell}
import pony.brain._

import scala.jdk.CollectionConverters._
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

class PhotonCannon(unit: APIUnit)
  extends AnyUnit(unit) with Building with GroundAndAirWeapon with NormalAirDamage with NeedsPower with NormalGroundDamage with ArmedBuildingCoveringGroundAndAir with DetectorBuilding {
  override def damageDelayFactorAir = 1

  override def damageDelayFactorGround = 1
}
