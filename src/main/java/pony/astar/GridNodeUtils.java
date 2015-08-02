/* Copyright H-Star Development 2007 */
package pony.astar;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 26.12.2007 @ 02:21:58
 */
public class GridNodeUtils {
    @SuppressWarnings({"AssignmentToMethodParameter"})
    public static boolean isDirectLineWalkable(int p_x0,
                                               int p_y0,
                                               final int p_x1,
                                               final int p_y1,
                                               final SearchContext<Node[][]> p_ctx) {
        int l_dy = p_y1 - p_y0;
        int l_dx = p_x1 - p_x0;
        final int l_stepy;

        if (l_dy < 0) {
            l_dy = -l_dy;
            l_stepy = -1;
        } else {
            l_stepy = 1;
        }
        final int l_stepx;
        if (l_dx < 0) {
            l_dx = -l_dx;
            l_stepx = -1;
        } else {
            l_stepx = 1;
        }
        l_dy <<= 1;
        l_dx <<= 1;

        final Node[][] l_allNodes = p_ctx.getAllNodes();
        if (Node.isNotWalkable(l_allNodes[p_x0][p_y0])) {
            return false;
        }
        if (l_dx > l_dy) {
            int fraction = l_dy - (l_dx >> 1);
            while (p_x0 != p_x1) {
                if (fraction >= 0) {
                    p_y0 += l_stepy;
                    fraction -= l_dx;
                }
                p_x0 += l_stepx;
                fraction += l_dy;
                if (Node.isNotWalkable(l_allNodes[p_x0][p_y0])) {
                    return false;
                }
            }
        } else {
            int fraction = l_dx - (l_dy >> 1);
            while (p_y0 != p_y1) {
                if (fraction >= 0) {
                    p_x0 += l_stepx;
                    fraction -= l_dy;
                }
                p_y0 += l_stepy;
                fraction += l_dx;
                if (Node.isNotWalkable(l_allNodes[p_x0][p_y0])) {
                    return false;
                }
            }
        }
        return true;
    }

    public static void connectNodes(
            @NotNull
            final Node[][] p_grid) {
        final int l_sizeX = p_grid.length;
        final int l_sizeY = p_grid[0].length;
        for (int l_x = 0; l_x < l_sizeX; l_x++) {
            for (int l_y = 0; l_y < l_sizeY; l_y++) {
                final Node l_current = p_grid[l_x][l_y];
                if (l_current != null) {
                    for (int l_x2 = -1; l_x2 <= 1; l_x2++) {
                        for (int l_y2 = -1; l_y2 <= 1; l_y2++) {
                            if (!(l_x2 == 0 && l_y2 == 0)) {
                                if (l_x + l_x2 < l_sizeX && l_y + l_y2 < l_sizeY && l_x + l_x2 >= 0 &&
                                    l_y + l_y2 >= 0) {
                                    final Node l_neighbor = p_grid[l_x + l_x2][l_y + l_y2];
                                    if (l_neighbor != null) {
                                        l_current.addNode(l_neighbor);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    @SuppressWarnings({"MethodWithMultipleLoops"})
    public static Node[] collectNewAndOpenNeighbors(
            @NotNull
            final Node[][] p_grid,
            final int p_x,
            final int p_y,
            @NotNull
            final Node[] p_fill, final boolean skipDiagonalConnections) {
        int l_fillIndex = 0;
        final int l_sizeX = p_grid.length;
        final int l_sizeY = p_grid[0].length;
        for (int l_x2 = -1; l_x2 <= 1; l_x2++) {
            for (int l_y2 = -1; l_y2 <= 1; l_y2++) {
                if (!(l_x2 == 0 && l_y2 == 0)) {
                    if (!skipDiagonalConnections || (l_x2 == 0 || l_y2 == 0)) {
                        if (p_x + l_x2 < l_sizeX && p_y + l_y2 < l_sizeY && p_x + l_x2 >= 0 && p_y + l_y2 >= 0) {
                            final Node l_neighbor = p_grid[p_x + l_x2][p_y + l_y2];
                            if (l_neighbor != null && l_neighbor.getState() != Node.NodeState.CLOSED) {
                                //noinspection ValueOfIncrementOrDecrementUsed
                                p_fill[l_fillIndex++] = l_neighbor;
                            }
                        }
                    }
                }
            }
        }
        if (l_fillIndex < 8) {
            //noinspection AssignmentToNull
            p_fill[l_fillIndex] = null;
        }
        return p_fill;
    }

    public static Node[] collectWalkableNeighbors(
            @NotNull
            final Node[][] p_grid,
            final int p_x,
            final int p_y,
            @NotNull
            final Node[] p_fill) {
        int l_fillIndex = 0;
        final int l_sizeX = p_grid.length;
        final int l_sizeY = p_grid[0].length;
        for (int l_x2 = -1; l_x2 <= 1; l_x2++) {
            for (int l_y2 = -1; l_y2 <= 1; l_y2++) {
                if (!(l_x2 == 0 && l_y2 == 0)) {
                    if (p_x + l_x2 < l_sizeX && p_y + l_y2 < l_sizeY && p_x + l_x2 >= 0 && p_y + l_y2 >= 0) {
                        final Node l_neighbor = p_grid[p_x + l_x2][p_y + l_y2];
                        if (Node.isWalkable(l_neighbor)) {
                            p_fill[l_fillIndex++] = l_neighbor;
                        }
                    }
                }
            }
        }
        return p_fill;
    }

    public static void asImage(final Node[][] p_grid) throws IOException {
        final BufferedImage l_bi = new BufferedImage(p_grid.length, p_grid[0].length, BufferedImage.TYPE_INT_ARGB);
        for (int l_x = 0; l_x < p_grid.length; l_x++) {
            final Node[] l_nodes = p_grid[l_x];
            for (int l_y = 0; l_y < l_nodes.length; l_y++) {
                final Node l_node = l_nodes[l_y];
                if (Node.isNotWalkable(l_node)) {
                    l_bi.setRGB(l_x, l_y, Color.WHITE.getRGB());
                }
            }
        }
        ImageIO.write(l_bi, "png", new File("gridasimage.png"));
    }

    @Nullable
    public static <T extends Node> T findNearestWalkable(final int p_startX,
                                                         final int p_startY,
                                                         @NotNull
                                                         final T[][] p_allNodes,
                                                         final int p_maxRadius) {
        final T l_start = p_allNodes[p_startX][p_startY];
        if (Node.isWalkable(l_start)) {
            return l_start;
        }
        int l_radius = 1;
        do {
            for (int l_x = -l_radius; l_x <= l_radius; l_x++) {
                final int l_absX = p_startX + l_x;
                if (l_absX < 0 || l_absX >= p_allNodes.length) {
                    continue;
                }
                for (int l_y = -l_radius; l_y <= l_radius; l_y++) {
                    final int l_absY = p_startY + l_y;
                    if (l_absY < 0 || l_absY >= p_allNodes[l_absX].length) {
                        continue;
                    }
                    final T l_retCandidate = p_allNodes[l_absX][l_absY];
                    if (l_retCandidate != null) {
                        return l_retCandidate;
                    }
                }
            }
            l_radius++;
        }
        while (l_radius < p_maxRadius);
        return null;
    }

    public static void removeSmallAreas(final Node[][] p_grid) {
        int l_biggestAreaSize = 0;
        final boolean[][] l_checked = new boolean[p_grid.length][p_grid[0].length];
        final Collection<Set<Node>> l_areaSizePerNode = new HashSet<Set<Node>>();
        for (int i = 0; i < p_grid.length; i++) {
            final Node[] l_nodes = p_grid[i];
            for (int j = 0; j < l_nodes.length; j++) {
                if (!l_checked[i][j]) {
                    l_checked[i][j] = true;
                    final Node l_node = l_nodes[j];
                    if (l_node != null) {
                        final Set<Node> l_area = getArea(l_node);
                        if (!l_area.isEmpty()) {
                            for (final Node l_node1 : l_area) {
                                l_checked[l_node1.getX()][l_node1.getY()] = true;
                            }
                            l_biggestAreaSize = Math.max(l_biggestAreaSize, l_area.size());
                            l_areaSizePerNode.add(l_area);
                        }
                    }
                }
            }
        }
        for (final Set<Node> l_nodes : l_areaSizePerNode) {
            if (l_nodes.size() < l_biggestAreaSize) {
                for (final Node l_node : l_nodes) {
                    //noinspection AssignmentToNull
                    p_grid[l_node.getX()][l_node.getY()] = null;
                }
            }
        }

    }

    private static Set<Node> getArea(final Node p_node) {
        final HashSet<Node> l_area = new HashSet<Node>();
        getArea(p_node, l_area);
        return l_area;
    }

    private static void getArea(final Node p_node, final HashSet<Node> p_nodes) {
        if (p_node != null) {
            final Set<Node> l_outer = new HashSet<Node>();
            final Set<Node> l_outerDelta = new HashSet<Node>();
            l_outer.add(p_node);
            int l_oldSize = 0;
            do {
                l_oldSize = p_nodes.size();
                for (Iterator<Node> it = l_outer.iterator(); it.hasNext(); ) {
                    final Node l_node = it.next();
                    final boolean l_added = p_nodes.add(l_node);
                    if (l_added) {
                        final Node[] l_nodes = l_node.getNodes();
                        for (int i = 0; i < l_nodes.length; i++) {
                            final Node l_node0 = l_nodes[i];
                            if (l_node0 != null) {
                                l_outerDelta.add(l_node0);
                            } else {
                                break;
                            }
                        }
                    }
                }
                l_outer.clear();
                l_outer.addAll(l_outerDelta);
                l_outerDelta.clear();

            }
            while (p_nodes.size() != l_oldSize);
        }
    }
}
