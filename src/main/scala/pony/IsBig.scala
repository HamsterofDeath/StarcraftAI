package pony

trait IsBig extends Mobile with CanDie {
  override val armorType = Large
}
