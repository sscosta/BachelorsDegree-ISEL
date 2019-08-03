package org.isel.jingle;

import org.isel.jingle.misc.StreamUtils;
import org.junit.Test;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toList;
import static junit.framework.Assert.assertEquals;
import static org.isel.jingle.misc.StreamUtils.cache;
import static org.junit.Assert.assertArrayEquals;

public class StreamUtilsTest {

    private final List<String> seq1 = Arrays.asList("isel", "ola", "dup", "super", "jingle");
    private final List<Integer> seq2Asc = Arrays.asList(4,5,6,7);
    private final List<Integer> seq2AscDupl = Arrays.asList(4,5,6,7,4,5,6,7);
    private final List<Integer> seq2Desc = Arrays.asList(7,6,5,4);
    private final List<Integer> seq2DescDupl = Arrays.asList(7,6,5,4,7,6,5,4);
    private final List<String> expected = Arrays.asList("isel4", "ola0", "dup0", "super5", "jingle6");

    @Test
    public void cacheTest(){
        Stream<Integer> nrs = Stream.generate(() -> (new Random()).nextInt(100));

        Supplier<Stream<Integer>> cache = cache(nrs.limit(100));
        Object[] expected = cache.get().toArray();
        Object[] actual = cache.get().toArray();

        assertStreamEquals(cache.get(),cache.get());
        assertArrayEquals(expected, actual);
    }

    @Test
    public void shouldMergeSequencesWithoutDuplicatesOnSeq2Ascending() {
        final List<String> merged = merge(seq2Asc);

        assertEquals(expected, merged);
    }

    @Test
    public void shouldMergeSequencesWithoutDuplicatesOnSeq2AscendingWithDuplicates() {
        final List<String> merged = merge(seq2AscDupl);
        assertEquals(expected, merged);
    }

    @Test
    public void shouldMergeSequencesWithDuplicatesOnSeq2Descending() {
        final List<String> merged = merge(seq2Desc);

        assertEquals(Arrays.asList("isel4", "ola0", "dup0", "super5", "jingle6"), merged);
    }

    @Test
    public void shouldMergeSequencesWithDuplicatesOnSeq2DescendingWithDuplicates() {
        final List<String> merged = merge(seq2DescDupl);

        assertEquals(Arrays.asList("isel4", "ola0", "dup0", "super5", "jingle6"), merged);
    }

    private List<String> merge(List<Integer> seq2) {
        return StreamUtils.merge(seq1.stream(), seq2.stream(), (str, nr) -> str.length() == nr, (str, nr) -> str + nr, 0).collect(toList());
    }

    static void assertStreamEquals(Stream<?> s1, Stream<?> s2)
    {
        Iterator<?> iter1 = s1.iterator(), iter2 = s2.iterator();
        while(iter1.hasNext() && iter2.hasNext())
            assertEquals(iter1.next(), iter2.next());
        assert !iter1.hasNext() && !iter2.hasNext();
    }
}
