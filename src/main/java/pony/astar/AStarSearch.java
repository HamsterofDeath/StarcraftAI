/* Copyright H-Star Development 2007 */
package pony.astar;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import pony.util.CollectionUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;

/**
 * Developed with pleasure :)<br>
 *
 * @author HamsterofDeath Created 22.12.2007 @ 16:25:24
 */
public class AStarSearch {
    public static AStarSearch instance;
    @NotNull
    final         Heuristics  m_heuristics;
    @NotNull
    private final Node        m_start;
    @NotNull
    private final Node        m_target;
    @NotNull
    private final Collection<Node> m_allOpened = new ArrayList<Node>(100000);
    @Nullable
    private final SearchContext m_ctx;
    private       Comparator<Node>    m_estimationCmp = new Comparator<Node>() {
        @SuppressWarnings({"MethodWithMultipleReturnPoints"})
        public int compare(final Node o1, final Node o2) {
            final int l_cost2 = o2.getEstimatedTotalCost(m_target, m_heuristics);
            final int l_cost1 = o1.getEstimatedTotalCost(m_target, m_heuristics);
            if (l_cost1 > l_cost2) {
                return 1;
            } else {
                if (l_cost2 > l_cost1) {
                    return -1;
                } else {
                    return 0;
                }
            }
        }
    };
    @NotNull
    private final PriorityQueue<Node> m_open          = new PriorityQueue<Node>(8192, m_estimationCmp);
    @Nullable
    private List<Node>        m_solution;
    private SearchResultState m_searchResultState;
    private Node              m_bestFound;

    public AStarSearch(
            @NotNull
            final Node p_start,
            @NotNull
            final Node p_target,
            @Nullable
            final Heuristics p_heuristics) {
        this(p_start, p_target, p_heuristics, null);
    }

    public AStarSearch(
            @NotNull
            final Node p_start,
            @NotNull
            final Node p_target,
            @NotNull
            final SearchContext p_ctx) {
        this(p_start, p_target, null, p_ctx);
    }

    public AStarSearch(
            @NotNull
            final Node p_start,
            @NotNull
            final Node p_target,
            @Nullable
            final Heuristics p_heuristics,
            @Nullable
            final SearchContext p_ctx) {
        super();
        m_ctx = p_ctx;
        instance = this;
        m_start = p_start;
        m_target = p_target;
        if (p_heuristics == null) {
            m_heuristics = Heuristics.NONE;
        } else {
            m_heuristics = p_heuristics;
        }
    }

    public AStarSearch(
            @NotNull
            final Node p_start,
            @NotNull
            final Node p_target) {
        this(p_start, p_target, p_start.suggestHeuristics());
    }

    @SuppressWarnings({"ConstantConditions"})
    public AStarSearch performSearch() {
        init();
        {
            m_searchResultState = SearchResultState.RUNNING;
            while (m_searchResultState == SearchResultState.RUNNING) {

                @Nullable
                final Node l_bestCandidate = m_bestFound = m_open.peek();
                if (l_bestCandidate == null) {
                    m_searchResultState = SearchResultState.NOT_SOLVABLE;
                } else {
                    l_bestCandidate.close();
                    m_open.remove();
                    if (l_bestCandidate == m_target) {
                        m_searchResultState = SearchResultState.SOLUTION_FOUND;
                    } else {
                        final Node[] l_nodes = l_bestCandidate.getNodes();
                        for (final Node l_node : l_nodes) {
                            //for performance reasons, null means "no more elements". this way, a fixed size
                            // array can be (re)used
                            if (l_node == null) {
                                break;
                            } else {
                                switch (l_node.getState()) {
                                    case CLOSED:
                                        break;
                                    case OPEN:
                                        if (l_node.getWholePathCost() > l_bestCandidate.getWholePathCost() +
                                                                        l_node.evalCostFromParent(l_bestCandidate)) {
                                            m_open.remove(l_node);
                                            l_node.setNewParent(l_bestCandidate);
                                            m_open.add(l_node);
                                        }
                                        break;
                                    case UNKNOWN:
                                        open(l_node, l_bestCandidate);
                                        break;
                                }
                            }
                        }
                        if (m_open.isEmpty()) {
                            m_searchResultState = SearchResultState.NOT_SOLVABLE;
                        }
                    }
                }
            }
        }

        if (m_target.getParent() != null) {
            final List<Node> l_list = CollectionUtils.asList(m_target.iterator());
            Collections.reverse(l_list);
            optimizePath(l_list);
            m_solution = l_list;
        } else {
            final List<Node> l_list = CollectionUtils.asList(m_bestFound.iterator());
            Collections.reverse(l_list);
            optimizePath(l_list);
            m_solution = l_list;
        }

        for (final Node l_node : m_allOpened) {
            l_node.initForSearch();
        }
        m_allOpened.clear();
        return this;
    }

    private void optimizePath(final List<Node> p_list) {
        if (!p_list.isEmpty()) {
            if (p_list.get(0).supportsShortcuts()) {
                for (int i = 0; i < p_list.size() - 2; i++) {
                    final Node l_node = p_list.get(i);
                    while (p_list.size() - i > 2 && l_node.canReachDirectly(p_list.get(i + 2), m_ctx)) {
                        p_list.remove(i + 1);
                    }
                }
            }
        }
    }

    private void init() {
        m_solution = null;
        m_open.clear();

        start();
    }

    private void start() {
        m_start.initAsStart();
        m_open.add(m_start);
        m_allOpened.add(m_start);
    }

    private void open(final Node p_node, final Node p_parent) {
        m_allOpened.add(p_node);
        p_node.open();
        p_node.setNewParent(p_parent);
        m_open.add(p_node);
    }

    @NotNull
    public Heuristics getHeuristics() {
        return m_heuristics;
    }

    @Nullable
    public Node getTargetOrNearestReachable() {
        if (m_searchResultState == SearchResultState.SOLUTION_FOUND) {
            return m_target;
        } else if (m_searchResultState == SearchResultState.NOT_SOLVABLE && m_bestFound != null) {
            return m_bestFound;
        }
        return null;
    }

    public int evalCostToGoal(final Node p_tNode) {
        return m_heuristics.estimateCost(p_tNode, m_target);
    }

    @NotNull
    public Node getTarget() {
        return m_target;
    }

    @Nullable
    public List<Node> getSolution() {
        return m_solution;
    }

    public boolean isFinished() {
        return SearchResultState.SOLUTION_FOUND == m_searchResultState ||
               SearchResultState.NOT_SOLVABLE == m_searchResultState;
    }

    public boolean isSolved() {
        return SearchResultState.SOLUTION_FOUND == m_searchResultState;
    }

    public boolean isUnsolvable() {
        return SearchResultState.NOT_SOLVABLE == m_searchResultState;
    }

    private enum SearchResultState {
        RUNNING,
        SOLUTION_FOUND,
        NOT_SOLVABLE
    }


}
