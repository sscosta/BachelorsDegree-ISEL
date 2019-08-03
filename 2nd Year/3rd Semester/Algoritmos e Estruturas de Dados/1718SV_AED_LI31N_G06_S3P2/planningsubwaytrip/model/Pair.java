package series.serie3.planningsubwaytrip.model;

public class Pair<K,V> {
    K k;
    V v;
    public Pair<K,V> next;
    public Pair(K key,V value){
        k=key;
        v=value;
    }

    public V getValue() {
        return v;
    }
    public K getKey(){return k;}
    public void setValue(V newVal){
        v=newVal;
    }

    public Pair<K, V> getNext() {
        return next;
    }
}
