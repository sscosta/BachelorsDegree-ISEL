package org.isel.jingle.misc;

import org.isel.jingle.service.model.TrackRank;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.*;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class StreamUtils<T> {

    private StreamUtils(){
    }

    public static <T> Supplier<Stream<T>>cache(Stream<T> src){
        Spliterator<T> spliterator = src.spliterator();
        List<T> list = new ArrayList<T>();
        return () -> StreamSupport.stream(new SpliteratorCache<T>(spliterator,list),false);
    }

    public static <T,U,R> Stream<R> merge(Stream<T> seq1, Stream<U> seq2, BiPredicate<T,U> pred, BiFunction<T,U,R> transf, U defaultVal){
        Supplier<Stream<U>> cachedSeq2 = cache(seq2);
        return seq1.map(s1Elem -> transf.apply(s1Elem, cachedSeq2.get().filter( s2Elem -> pred.test(s1Elem,s2Elem)).findFirst().orElse(defaultVal)));
    }
    public static <T> Predicate<T> distinctByKey(Function<? super T, ?> keyExtractor) {
        Set<Object> seen = ConcurrentHashMap.newKeySet();
        return t -> seen.add(keyExtractor.apply(t));
    }

    public static TrackRank highestRank(TrackRank tr, TrackRank tr2) {
        int rank1 = tr.getRank();
        int rank2 = tr2.getRank();
        if(rank1 != 0 && rank2!=0 && rank1 < rank2)
            return tr;
        else if(rank1!=0) return tr;
        return tr2;
    }
}
