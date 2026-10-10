package pony
package brain
package modules
package bunkers

import pony.geometry.{Area, MapPosition, MapTilePosition}
import pony.terrain.AreaHelper

import scala.collection.mutable

private[pony] object BunkerCoverage {

  /** Match the existing mineral-path mask: every patch lane plus actual depot return approaches. */
  def workerTiles(patches: Seq[Area], depots: Seq[Area], routes: Seq[Seq[MapTilePosition]]): Vector[MapTilePosition] = {
    val tiles = mutable.Set.empty[MapTilePosition]
    patches.foreach(p => tiles ++= p.growBy(1).tiles)
    depots.foreach { depot =>
      tiles ++= depot.outline
      tiles ++= depot.growBy(1).outline
    }
    routes.foreach { path =>
      tiles ++= path
      path.sliding(2).foreach { pair =>
        if (pair.size == 2)
          AreaHelper.traverseTilesOfLine(pair.head, pair.last, (x, y) => tiles += MapTilePosition(x, y))
      }
    }
    tiles.toVector.sortBy(p => (p.y, p.x))
  }
  // BWAPI 4.1 Position.h: native approximation; ignoring collision extents is conservative.
  def approximateDistance(a: MapPosition, b: MapPosition): Int = {
    val dx    = math.abs(a.x - b.x); val dy = math.abs(a.y - b.y)
    val small = dx min dy; val large        = dx max dy
    if (small < (large >> 2)) large
    else { val m = (3 * small) >> 3; (m >> 5) + m + large - (large >> 4) - (large >> 6) }
  }
  def ready(points: Vector[MapPosition], sites: Vector[Area], completedCargo: Map[MapTilePosition, Int], range: Int) =
    sites.nonEmpty && sites.forall(s => completedCargo.get(s.upperLeft).contains(4)) &&
      points.forall(p => sites.exists(covers(_, p, range)))
  def corners(tiles: Seq[MapTilePosition]): Vector[MapPosition] = tiles.flatMap { t =>
    Seq(
      MapPosition(t.mapX, t.mapY),
      MapPosition(t.mapX + 32, t.mapY),
      MapPosition(t.mapX, t.mapY + 32),
      MapPosition(t.mapX + 32, t.mapY + 32)
    )
  }.distinct.toVector
  def covers(site: Area, point: MapPosition, range: Int): Boolean = {
    val center = MapPosition(site.upperLeft.mapX + site.width * 16, site.upperLeft.mapY + site.height * 16)
    approximateDistance(center, point) <= range
  }
  private def separate(a: Area, b: Area) = !a.growBy(1).tiles.exists(b.tiles.toSet)
  private def overlaps(a: Area, b: Area) = a.tiles.exists(b.tiles.toSet)
  def select(
      points: Vector[MapPosition],
      candidates: Vector[Area],
      existing: Vector[Area],
      range: Int,
      safeTogether: Seq[Area] => Boolean = _ => true,
      relaxedFallback: Boolean = false
  ): Vector[Area] = {
    val needed = points.indices.filterNot(i => existing.exists(covers(_, points(i), range))).toSet
    val ranked = candidates.filter(c => existing.forall(separate(c, _))).map { c =>
      c -> needed.filter(i => covers(c, points(i), range))
    }.filter(_._2.nonEmpty).sortBy { case (c, hit) => (-hit.size, c.upperLeft.y, c.upperLeft.x) }
    if (needed.isEmpty) Vector.empty
    else ranked.find(e => e._2 == needed && safeTogether(Vector(e._1))).map(e => Vector(e._1)).getOrElse {
      val pair = ranked.indices.iterator.flatMap { i =>
        val left = needed -- ranked(i)._2
        (i + 1 until ranked.size).iterator.filter(j =>
          separate(ranked(i)._1, ranked(j)._1) &&
            left.subsetOf(ranked(j)._2) && safeTogether(Vector(ranked(i)._1, ranked(j)._1))
        )
          .map(j => Vector(ranked(i)._1, ranked(j)._1))
      }.take(1).toVector.headOption
      pair.getOrElse {
        def greedy(requireSafety: Boolean): Vector[Area] = {
          var left     = needed
          var selected = Vector.empty[Area]
          while (left.nonEmpty) {
            // Preserve the same first admissible ranked site; whole-map connectivity is expensive.
            // The relaxed pass only forbids direct footprint overlap; tiles between cramped
            // mining lanes do not allow the full one-tile buffer on every start.
            def admissible(c: Area) =
              if (requireSafety) selected.forall(separate(c, _))
              else selected.forall(!overlaps(c, _))
            val options = ranked.filter(e => admissible(e._1))
              .map(e => (e._1, e._2 intersect left)).filter(_._2.nonEmpty)
              .sortBy(e => (-e._2.size, e._1.upperLeft.y, e._1.upperLeft.x))
            val next = if (requireSafety) options.iterator.find(e => safeTogether(selected :+ e._1))
            else options.headOption
            if (next.isEmpty) return Vector.empty
            selected :+= next.get._1
            left --= next.get._2
          }
          selected
        }
        val strict = greedy(requireSafety = true)
        if (strict.nonEmpty || !relaxedFallback) strict else greedy(requireSafety = false)
      }
    }
  }
}
