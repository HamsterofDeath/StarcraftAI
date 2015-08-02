package pony.util;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * Created by HoD on 01.08.2015.
 */
public class CollectionUtils {
    public static <T> List<T> asList(Iterator<T> it) {
        final List<T> ret = new ArrayList<>();
        it.forEachRemaining(ret::add);
        return ret;
    }

    public static <T> boolean isEmpty(final T[] nodes) {
        return nodes == null || nodes.length > 0;
    }

    public static <T> boolean containsElements(final T[] nodes) {
        return !isEmpty(nodes) && nodes[0] != null;
    }
}
