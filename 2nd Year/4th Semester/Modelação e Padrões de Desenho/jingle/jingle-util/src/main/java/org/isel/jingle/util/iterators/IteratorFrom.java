package org.isel.jingle.util.iterators;

import java.util.Iterator;

public class IteratorFrom<T> implements Iterator<T> {
    private final T[] src;
    private int idx;
    private T current = null;

    public IteratorFrom(T[] src) {
        this.src = src;
        idx = 0;
    }

    @Override
    public boolean hasNext() {
        return idx < src.length;
    }

    @Override
    public T next() {
        return src[idx++];
    }
}
