package pony.astar.test;
/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath
 * Created 22.12.2007 @ 23:48:51
 */

import pony.astar.AStarSearch;
import pony.astar.GridNodeUtils;
import pony.astar.LightGridNode2D;
import pony.astar.Node;
import pony.astar.SearchContext;

import javax.imageio.ImageIO;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Iterator;

public class AStarSearchTest {
    AStarSearch m_aStarSearch;

    public static void main(String[] args) throws IOException {
        new AStarSearchTest().testLowMemSearch();
    }

    public void testLowMemSearch() throws IOException {
        //BufferedImage l_bi = new BufferedImage(l_size, l_size, BufferedImage.TYPE_BYTE_GRAY);
        //final BufferedImage l_bi = ImageIO.read(AStarSearchTest.class.getResource("maze_200x200.png"));
        final BufferedImage l_bi = ImageIO.read(AStarSearchTest.class.getResource("example_maze_1.png"));
        final int l_sizeX = l_bi.getWidth();
        final int l_sizeY = l_bi.getHeight();
        final LightGridNode2D[][] l_node2Ds = new LightGridNode2D[l_sizeX][l_sizeY];
        for (short l_x = 0; l_x < l_sizeX; l_x++) {
            for (short l_y = 0; l_y < l_sizeY; l_y++) {
                l_node2Ds[l_x][l_y] = new LightGridNode2D(l_x, l_y);
            }
        }
        for (int l_x = 0; l_x < l_sizeX; l_x++) {
            for (int l_y = 0; l_y < l_sizeY; l_y++) {
                if (l_bi.getRGB(l_x, l_y) != Color.white.getRGB()) {
                    //noinspection AssignmentToNull
                    l_node2Ds[l_x][l_y] = null;
                }
            }
        }
        final SearchContext<LightGridNode2D[][]> l_context = new SearchContext<LightGridNode2D[][]>(l_node2Ds);
        LightGridNode2D.initContext(l_context);
        //GridNodeUtils.removeSmallAreas(l_node2Ds);
        System.gc();
        System.out.println(
                "Nodes built!, needed ram: " + (Runtime.getRuntime().maxMemory() - Runtime.getRuntime().freeMemory()));
        m_aStarSearch = new AStarSearch(l_node2Ds[1][1], l_node2Ds[l_sizeX - 2][l_sizeY - 2], l_context);
        final long time = System.currentTimeMillis();
        m_aStarSearch.performSearch();
        System.out.println(System.currentTimeMillis() - time);
        LightGridNode2D l_previous = null;
        for (Iterator<Node> l_iterator = m_aStarSearch.getSolution().iterator(); l_iterator.hasNext(); ) {
            final LightGridNode2D l_gridNode2D = (LightGridNode2D) l_iterator.next();
            l_bi.setRGB(l_gridNode2D.getX(), l_gridNode2D.getY(), Color.YELLOW.getRGB());
            if (l_previous != null) {
                final Graphics l_graphics = l_bi.getGraphics();
                l_graphics.setColor(Color.DARK_GRAY);
                l_graphics.drawLine(l_previous.getX(), l_previous.getY(), l_gridNode2D.getX(), l_gridNode2D.getY());
            }
            l_previous = l_gridNode2D;
        }
        GridNodeUtils.asImage(l_node2Ds);

        m_aStarSearch.getTargetOrNearestReachable().iterator();
        for (Iterator<Node> l_iterator = m_aStarSearch.getTargetOrNearestReachable().iterator();
             l_iterator.hasNext(); ) {
            final LightGridNode2D l_gridNode2D = (LightGridNode2D) l_iterator.next();
            l_bi.setRGB(l_gridNode2D.getX(), l_gridNode2D.getY(), Color.RED.getRGB());
        }
        final File file = new File("output.png");
        System.out.println("Check out "+file.getAbsolutePath());
        ImageIO.write(l_bi, "PNG", new FileOutputStream(file));
    }


}