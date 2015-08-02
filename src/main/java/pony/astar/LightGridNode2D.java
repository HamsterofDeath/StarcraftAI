/* Copyright H-Star Development 2007 */
package pony.astar;

import org.jetbrains.annotations.NotNull;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 22.12.2007 @ 23:51:09
 */
@SuppressWarnings({"AccessingNonPublicFieldOfAnotherObject"})
public class LightGridNode2D extends Node {
    private static final int    MS_READ_Y        = 65535;
    private static final long   serialVersionUID = 0L;
    @NotNull
    private static final Node[] ms_tmp           = new Node[8];
    protected static SearchContext ms_sharedcontext;
    private static boolean ms_skipDiagonalConnections = false;
    private          int           m_position;

    public LightGridNode2D(final short p_x, final short p_y) {
        m_position |= ((int) p_x) << 16;
        m_position |= ((int) p_y);
    }

    public static void setSkipDiagonalConnections(final boolean skipDiagonalConnections) {
        ms_skipDiagonalConnections = skipDiagonalConnections;
    }

    public static void initContext(final SearchContext p_ctx) {
        ms_sharedcontext = p_ctx;
    }

    public static SearchContext getContext() {
        return ms_sharedcontext;
    }

    public int evalCostFromParent(
            @NotNull
            final Node p_parent) {
        final LightGridNode2D l_parent = (LightGridNode2D) p_parent;
        if (l_parent.m_position >>> 16 != m_position >>> 16 &&
            (l_parent.m_position & MS_READ_Y) != (m_position & MS_READ_Y)) {
            return 141;
        }
        return 100;
    }

    private int getDistanceTo(
            @NotNull
            final LightGridNode2D p_other) {
        final int l_dstX = (p_other.m_position >>> 16) - (m_position >>> 16);
        final int l_dstY = (p_other.m_position & MS_READ_Y) - (m_position & MS_READ_Y);
        return (int) (((float) Math.sqrt(l_dstX * l_dstX + l_dstY * l_dstY)) * 100.0F);
    }

    public Node[] getNodes() {
        final SearchContext<LightGridNode2D[][]>
                l_searchContext =
                (SearchContext<LightGridNode2D[][]>) ms_sharedcontext;
        final Node[][] l_node2Ds = l_searchContext.getAllNodes();
        return GridNodeUtils.collectNewAndOpenNeighbors(l_node2Ds, m_position >>> 16, m_position & MS_READ_Y, ms_tmp,
                                                        ms_skipDiagonalConnections);

    }

    public Heuristics<LightGridNode2D> suggestHeuristics() {
        return new Heuristics<LightGridNode2D>() {
            public int estimateCost(
                    @NotNull
                    final LightGridNode2D p_from,
                    @NotNull
                    final LightGridNode2D p_target) {
                return p_from.getDistanceTo(p_target);
            }
        };
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }

    @SuppressWarnings({"RefusedBequest"})
    @Override
    public boolean canReachDirectly(
            @NotNull
            final Node p_node, final SearchContext p_ctx) {
        final LightGridNode2D l_node = (LightGridNode2D) p_node;
        return GridNodeUtils.isDirectLineWalkable(m_position >>> 16, m_position & MS_READ_Y, l_node.m_position >>> 16,
                                                  l_node.m_position & MS_READ_Y, p_ctx);
    }

    public void addNode(
            @NotNull
            final Node p_node) {
        throw new UnsupportedOperationException();
    }

    public void remove(final Node p_node) {
        throw new UnsupportedOperationException();
    }

    @SuppressWarnings({"RefusedBequest"})
    @Override
    public boolean supportsShortcuts() {
        return true;
    }

    public int getX() {
        return m_position >>> 16;
    }

    public int getY() {
        return m_position & MS_READ_Y;
    }

    @Override
    public String toString() {
        return getX() + "/" + getY();
    }
}
