package pony
package brain
package modules

private[pony] object MineralFieldStaffing {
  def permitted(defaultCampaign: Boolean, landedField: Option[Int], miningField: Int) =
    !defaultCampaign || landedField.contains(miningField)
}
