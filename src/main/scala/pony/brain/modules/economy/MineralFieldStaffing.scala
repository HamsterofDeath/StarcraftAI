package pony
package brain
package modules
package economy

private[pony] object MineralFieldStaffing {
  def permitted(defaultCampaign: Boolean, landedField: Option[Int], miningField: Int) =
    !defaultCampaign || landedField.contains(miningField)
}
