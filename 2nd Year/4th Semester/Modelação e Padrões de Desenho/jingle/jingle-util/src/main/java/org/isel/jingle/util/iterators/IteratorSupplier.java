package org.isel.jingle.util.iterators;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class IteratorSupplier<T> extends BaseIterator<T> {

    private final Supplier<T> supplier;

    public IteratorSupplier(Supplier<T> supplier){
        this.supplier = supplier;
    }


    @Override
    protected boolean tryAdvance(Consumer<T> consumer) {
        consumer.accept(supplier.get());
        return true;
    }
}
