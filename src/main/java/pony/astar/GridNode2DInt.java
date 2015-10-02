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
public abstract class GridNode2DInt extends Node<GridNode2DInt> {
    private final int m_x;
    private final int m_y;
    private List<GridNode2DInt> m_nodes = new ArrayList<GridNode2DInt>(8);

    public GridNode2DInt(final int p_x, final int p_y) {
        this.m_x = p_x;
        this.m_y = p_y;
    }

    public int evalCostFromParent(
            @NotNull
            final GridNode2DInt p_parent) {

        return (int) Math.sqrt((double) (p_parent.m_x * this.m_x + p_parent.m_y * this.m_y));
    }

    public List<GridNode2DInt> getNodes() {
        return this.m_nodes;
    }


    public void remove() {
        for (GridNode2DInt m_node : m_nodes) {
            m_node.remove(this);
        }
    }

    public void addNode(
            @NotNull
            final GridNode2DInt p_node) {
        m_nodes.add(p_node);
    }

    public void remove(final GridNode2DInt p_node) {
        m_nodes.remove(p_node);
    }

    public int getX() {
        return this.m_x;
    }

    public int getY() {
        return this.m_y;
    }

}
