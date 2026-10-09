package pony

sealed trait ArmorType {
  val tileSize: Size
  def transportSize: Int
  def damageFactorIfHitBy(damageType: DamageType): DamageFactor
}

case object Small extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal     => Full
      case Concussive => Full
      case Explosive  => Half
      case _          => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 1
}

case object Medium extends ArmorType {
  override val tileSize = Size(1, 1)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal     => Full
      case Concussive => Half
      case Explosive  => ThreeQuarters
      case _          => !!!(s"Check $damageType")
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
      case Normal     => Full
      case Concussive => Quarter
      case Explosive  => Full
      case _          => !!!(s"Check $damageType")
    }
  }

  override def transportSize = 4
}

case object BuildingArmor extends ArmorType {
  override val tileSize = Size(4, 3)

  override def damageFactorIfHitBy(damageType: DamageType) = {
    damageType match {
      case Normal     => Full
      case Concussive => Quarter
      case Explosive  => Full
      case _          => !!!(s"Check $damageType")
    }
  }

  override def transportSize = !!!("This should never happen")
}
