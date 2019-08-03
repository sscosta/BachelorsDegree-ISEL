package org.isel.jingle.util.Iterables;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

public class IterableCache<T> implements Iterable<T> {

    private List<T> l = new ArrayList<>();
    private Iterator<T> it;
    private Iterable<T> src;

    public IterableCache(Iterable<T> src){
        this.src = src;
    }

    @Override
    public Iterator<T> iterator() {
        return new Iterator<T>() {
            int idx = 0;

            @Override
            public boolean hasNext() {
                if(it == null) it = src.iterator();
                return l.size() > idx || it.hasNext();
            }

            @Override
            public T next() {
                if (!hasNext())
                    throw new NoSuchElementException();
                if (l.size() > idx) return l.get(idx++);
                T val = it.next();
                l.add(idx, val);
                return l.get(idx++);
            }
        };
    }
}

