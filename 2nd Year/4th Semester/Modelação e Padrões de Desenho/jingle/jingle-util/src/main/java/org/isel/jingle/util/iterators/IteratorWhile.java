package org.isel.jingle.util.iterators;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class IteratorWhile<T> extends BaseIterator<T> {

    private Iterator<T> src;
    private final Predicate<T> pred;

    boolean stopped = false;
    private T val;

    public IteratorWhile(Iterable<T> src, Predicate<T> pred) {
        this.src = src.iterator();
        this.pred = pred;
    }

    @Override
    protected boolean tryAdvance(Consumer<T> consumer) {
        if(!src.hasNext() || stopped)
            return false;

        T aux = src.next();
        if(!pred.test(aux)) {
            stopped = true;
            return false;
        }
        consumer.accept(aux);
        return true;
    }
}
