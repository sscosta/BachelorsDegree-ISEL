package org.isel.jingle.util.iterators;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Function;

public class IteratorFlatMap<T, R> implements Iterator<R> {
    private final Iterator<T> src;
    private Iterator<R> subSrc = null;
    private final Function<T, Iterable<R>> mapper;
    private R current = null;

    public IteratorFlatMap(Iterable<T> src, Function<T, Iterable<R>> mapper) {
        this.mapper = mapper;
        this.src = src.iterator();
    }

    @Override
    public boolean hasNext() {
        if(subSrc!=null && subSrc.hasNext()){
            current = subSrc.next();
            return true;
        }
        if(src.hasNext()){
            subSrc = mapper.apply(src.next()).iterator();
            if(subSrc.hasNext()) {
                current = subSrc.next();
                return true;
            }
            else {
                return true;
            }
        }
        return false;
    }

    @Override
    public R next() {
        if (current==null && !hasNext()) throw new NoSuchElementException();
        R aux = current;
        current = null;
        return aux;
    }
}
