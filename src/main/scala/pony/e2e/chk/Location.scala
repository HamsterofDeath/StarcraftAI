package pony.e2e.chk

/** A named MRGN rectangle in pixels; `number` is the 1-based location number triggers use. */
final case class Location(number: Int, name: String, left: Int, top: Int, right: Int, bottom: Int) {
  require(number >= 1 && number <= 255 && number != Trigger.Anywhere, s"location $number is reserved or out of range")
}

object Location {
  def aroundTile(number: Int, name: String, tileX: Int, tileY: Int, radiusTiles: Int) =
    Location(
      number,
      name,
      (tileX - radiusTiles) * 32,
      (tileY - radiusTiles) * 32,
      (tileX + radiusTiles + 1) * 32,
      (tileY + radiusTiles + 1) * 32
    )
}
