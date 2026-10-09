package pony

trait IsMedium extends Mobile with CanDie {
  override val armorType = Medium
}
