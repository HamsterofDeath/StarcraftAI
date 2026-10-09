/* Copyright H-Star Development 2007 */
package pony.astar;

import org.jetbrains.annotations.NotNull;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 22.12.2007 @ 17:03:26
 */
public interface Heuristics<T extends Node<T>> {
    /**
     * @return a heuristic that estimates every remaining cost as zero, turning A* into Dijkstra
     */
    static <T extends Node<T>> Heuristics<T> none() {
        return (p_from, p_target) -> 0;
    }


    int estimateCost(
            @NotNull
            final T p_from,
            @NotNull
            final T p_target);
}
