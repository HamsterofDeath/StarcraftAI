package pony.astar;

import org.jetbrains.annotations.NotNull;

/**
 * Created by HoD on 28.09.2015.
 */
public interface NodeConnections<T extends Node<T>> {
    int evalCostFromParent(@NotNull T p_parent);
}
