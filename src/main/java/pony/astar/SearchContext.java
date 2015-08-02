/* Copyright H-Star Development 2007 */
package pony.astar;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 25.12.2007 @ 19:04:11
 */
public class SearchContext<T> implements Serializable {
    private static final long serialVersionUID = 0L;

    @NotNull
    T m_allNodes;

    public SearchContext(
            @NotNull
            final T p_allNodes) {
        super();
        m_allNodes = p_allNodes;
    }

    @NotNull
    public T getAllNodes() {
        return m_allNodes;
    }

    public void replaceNodes(T p_newNodes) {
        m_allNodes = p_newNodes;
    }


}
