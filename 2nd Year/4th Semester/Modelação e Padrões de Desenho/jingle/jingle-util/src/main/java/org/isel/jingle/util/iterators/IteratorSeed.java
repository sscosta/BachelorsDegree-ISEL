package org.isel.jingle.util.iterators;

import java.util.Iterator;
import java.util.function.Consumer;
import java.util.function.Function;

public class IteratorSeed<T> extends BaseIterator<T>{

    private final T seed;
    private final Function<T,T> func;

    private T current;

    public IteratorSeed(T seed, Function<T,T> func){
        this.func = func;
        this.seed = seed;
        current = seed;
    }

    @Override
    protected boolean tryAdvance(Consumer<T> consumer) {
        consumer.accept(current);
        current = func.apply(current);
        return true;
    }
}
