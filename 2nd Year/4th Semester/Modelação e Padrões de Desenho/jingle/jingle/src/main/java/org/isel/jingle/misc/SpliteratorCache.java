package org.isel.jingle.misc;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class SpliteratorCache<T> extends Spliterators.AbstractSpliterator<T> {

    private List<T> l;
    private final Spliterator<T> spliterator;
    private int idx;

    public SpliteratorCache(Spliterator<T> srcSpliterator, List<T> list){
        super(srcSpliterator.estimateSize(),srcSpliterator.characteristics());
        this.spliterator = srcSpliterator;
        this.l = list;
        this.idx = 0;
    }

    @Override
    public boolean tryAdvance(Consumer<? super T> action) {
        if (idx < l.size()) {
            action.accept(l.get(idx++));
            return true;
        }
        boolean advanded = spliterator.tryAdvance(t -> l.add(idx, t));
        if (advanded) {
            action.accept(l.get(idx++));
        }
        return advanded;
    }
}