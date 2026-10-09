package pony
package brain
package modules

private[pony] object ScoutPointPairs {
  def next(
      points: Vector[MapTilePosition],
      covered: Set[MapTilePosition]
  )(pathLength: (MapTilePosition, MapTilePosition) => Option[Double]): List[MapTilePosition] = {
    val uncovered = points.filterNot(covered)
    if (uncovered.size <= 1) uncovered.toList
    else uncovered.combinations(2).flatMap { pair =>
      pathLength(pair(0), pair(1)).map(length => pair.toList -> length)
    }.toVector.sortBy(_._2).headOption.map(_._1).getOrElse(Nil)
  }
}
