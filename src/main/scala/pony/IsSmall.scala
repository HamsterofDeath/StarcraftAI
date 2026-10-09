package pony

trait IsSmall extends Mobile with CanDie {
  override val armorType = Small
}
