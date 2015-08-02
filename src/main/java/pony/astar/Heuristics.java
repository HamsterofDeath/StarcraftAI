/* Copyright H-Star Development 2007 */
package pony.astar;

import org.jetbrains.annotations.NotNull;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 22.12.2007 @ 17:03:26
 */
public interface Heuristics<T extends Node> {
    Heuristics<Node> NONE = new Heuristics<Node>() {
        public int estimateCost(
                @NotNull
                final Node p_from,
                @NotNull
                final Node p_target) {
            return 0;
        }
    };


    int estimateCost(
            @NotNull
            final T p_from,
            @NotNull
            final T p_target);
}
