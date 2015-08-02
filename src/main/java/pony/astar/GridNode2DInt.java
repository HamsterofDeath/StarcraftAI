/*
 * Copyright (c) 2010. H-star Development
 */
package pony.astar;

import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 29.02.2008 @ 20:25:50
 */
public class GridNode2DInt extends Node {
    private final int m_x;
    private final int m_y;
    private List<GridNode2DInt> m_connections = new ArrayList<GridNode2DInt>(8);
    private Node[] m_nodes;

    public GridNode2DInt(final int p_x, final int p_y) {
        this.m_x = p_x;
        this.m_y = p_y;
    }

    public int evalCostFromParent(
            @NotNull
            final Node p_parent) {
        final GridNode2DInt l_other = (GridNode2DInt) p_parent;

        //noinspection AccessingNonPublicFieldOfAnotherObject,NumericCastThatLosesPrecision
        return (int) Math.sqrt((double) (l_other.m_x * this.m_x + l_other.m_y * this.m_y));
    }

    public Node[] getNodes() {
        if (this.m_nodes == null) {
            this.m_nodes = m_connections.toArray(new Node[m_connections.size()]);
        }
        return this.m_nodes;
    }

    public void remove() {
        assert this.m_nodes == null;
        assert this.m_connections != null;
        for (final GridNode2DInt l_connection : this.m_connections) {
            l_connection.remove(this);
        }
    }

    public void addNode(
            @NotNull
            final Node p_node) {
        assert this.m_nodes == null;
        assert this.m_connections != null;
        //noinspection SuspiciousMethodCalls
        assert !m_connections.contains(p_node);
        m_connections.add((GridNode2DInt) p_node);
    }

    public void remove(final Node p_node) {
        assert this.m_nodes == null;
        assert this.m_connections != null;
        //noinspection SuspiciousMethodCalls
        assert m_connections.contains(p_node);
        m_connections.remove(p_node);
    }

    public int getX() {
        return this.m_x;
    }

    public int getY() {
        return this.m_y;
    }

}
