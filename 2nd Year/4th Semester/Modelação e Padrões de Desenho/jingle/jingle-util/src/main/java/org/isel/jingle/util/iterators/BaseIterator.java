package org.isel.jingle.util.iterators;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.function.Consumer;

public abstract class BaseIterator<T> implements Iterator<T> {
    private Optional<T> current = Optional.empty();


    @Override
    public final boolean hasNext() {
        if (current.isPresent()) {
            return true;
        }

        return tryAdvance(t -> current = Optional.ofNullable(t));
    }


    @Override
    public final T next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }

        T ret = current.get();
        current = Optional.empty();
        return ret;
    }

    protected abstract boolean tryAdvance(Consumer<T> consumer);
}