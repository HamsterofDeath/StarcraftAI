/* Copyright H-Star Development 2007 */
package pony.astar;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import pony.util.CollectionUtils;

import java.io.Serializable;
import java.util.Iterator;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 22.12.2007 @ 16:12:03
 */
public abstract class Node implements Iterable, Serializable {
    private static final int  MS_MASK_GET_STATE    = 3;
    private static final int  MS_MASK_DELETE_STATE = ~3;
    private static final long serialVersionUID     = 0L;
    private static final NodeState[] NODE_STATES = NodeState.values();
    @Nullable
    protected Node m_parent;
    /**
     * 30 Bits Pathcost, 2 Bits state
     */
    protected int m_costAndState;

    protected static boolean isNotWalkable(
            @Nullable
            final Node p_node) {
        return p_node == null || CollectionUtils.isEmpty(p_node.getNodes());
    }

    public static boolean isWalkable(
            @Nullable
            final Node p_node) {
        return p_node != null && CollectionUtils.containsElements(p_node.getNodes());
    }

    /**
     * @param p_parent A directly known Node. Otherwise, the returned value is not defined.
     * @return
     */
    public abstract int evalCostFromParent(
            @NotNull
            final Node p_parent);

    public int getEstimatedTotalCost(final Node p_target, final Heuristics p_heuristics) {
        return (m_costAndState >>> 2) + p_heuristics.estimateCost(this, p_target);
    }

    public void setNewParent(
            @NotNull
            final Node p_parent) {
        assert isOpen();
        assert p_parent.isClosed();
        m_parent = p_parent;
        setPathCost((p_parent.m_costAndState >>> 2) + evalCostFromParent(p_parent));
    }

    public void initAsStart() {
        m_parent = null;
        setState(NodeState.OPEN);
    }

    public abstract pony.astar.Node[] getNodes();

    public boolean isClosed() {
        return getState() == NodeState.CLOSED;
    }

    public boolean isOpen() {
        return getState() == NodeState.OPEN;
    }

    public void close() {
        setState(NodeState.CLOSED);
    }

    @NotNull
    public NodeState getState() {
        return NODE_STATES[m_costAndState & MS_MASK_GET_STATE];
    }

    private void setState(final NodeState p_state) {
        m_costAndState = (m_costAndState & MS_MASK_DELETE_STATE) | p_state.ordinal();
        assert (m_costAndState & MS_MASK_GET_STATE) == p_state.ordinal();
    }

    public void open() {
        setState(NodeState.OPEN);
    }

    public Heuristics suggestHeuristics() {
        return null;
    }

    public abstract void remove();

    public void initForSearch() {
        setState(NodeState.UNKNOWN);
        m_parent = null;
        setPathCost(0);
    }

    private void setPathCost(final int p_cost) {
        assert p_cost <= Integer.MAX_VALUE >> 2;
        m_costAndState = (m_costAndState & MS_MASK_GET_STATE) | (p_cost << 2);
        assert (m_costAndState & MS_MASK_GET_STATE) < 3;
        assert m_costAndState >>> 2 == p_cost;
    }

    @Nullable
    public Node getParent() {
        return m_parent;
    }

    public boolean supportsShortcuts() {
        return false;
    }

    public boolean canReachDirectly(
            @NotNull
            final Node p_node, final SearchContext p_ctx) {
        throw new UnsupportedOperationException("Impl me!");
    }

    public abstract void addNode(
            @NotNull
            final Node p_node);

    public abstract void remove(final Node p_node);

    public int getWholePathCost() {
        return m_costAndState >>> 2;
    }

    /**
     * @return All parents in a row
     */
    @NotNull
    public Iterator<Node> iterator() {
        return new Iterator<Node>() {
            private Node m_current;
            private boolean m_endReached;

            public boolean hasNext() {
                if (m_endReached) {
                    return false;
                }
                if (m_current == null) {
                    m_current = Node.this;
                    if (m_current.m_parent == null) {
                        return false;
                    }
                } else {
                    m_endReached = (m_current.m_parent == null);
                }
                return !m_endReached;
            }

            public Node next() {
                return m_current = m_current.m_parent;
            }

            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    public abstract int getX();

    public abstract int getY();

    public enum NodeState {
        UNKNOWN,
        CLOSED,
        OPEN
    }
}
