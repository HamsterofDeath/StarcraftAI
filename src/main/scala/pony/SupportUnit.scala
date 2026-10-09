package pony

trait SupportUnit extends Mobile {
  private val myNearestAlliesWithWeapons = oncePer(Primes.prime47) {
    ownUnits.allMobilesWithWeapons.iterator.toVector.sortBy { other =>
      other.centerTile.distanceSquaredTo(centerTile)
    }
  }

  def nearestAllies = myNearestAlliesWithWeapons.get
}
