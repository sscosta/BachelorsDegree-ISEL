package org.isel.jingle.util.iterators;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.function.Consumer;

public class IteratorArray<T> extends BaseIterator<T>{

    private final T [] items;

    private int currIdx = 0;

    public IteratorArray(T[] items){
        this.items = items;
    }

    @Override
    protected boolean tryAdvance(Consumer<T> consumer) {
        if(currIdx < items.length) {
            consumer.accept(items[currIdx++]);
            return true;
        }
        return false;
    }
}
