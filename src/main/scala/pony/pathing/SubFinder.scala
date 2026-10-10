package pony
package pathing

import pony.geometry.MapTilePosition

trait SubFinder {
  def find: Option[MapTilePosition]
}
