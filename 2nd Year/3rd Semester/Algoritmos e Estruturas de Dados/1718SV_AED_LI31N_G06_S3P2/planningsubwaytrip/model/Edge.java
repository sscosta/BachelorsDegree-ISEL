package series.serie3.planningsubwaytrip.model;

public class Edge<Integer,E> {

    public int weight;
    public Edge next;
    Edge prev;
    public E connectsTo;
    public Edge(int theWeight,E theOneItConnectsTo,Edge theNext){
        weight=theWeight;
        connectsTo=theOneItConnectsTo;
        next=theNext;
    }

    public Edge getPrev() {
        return prev;
    }

    public int getWeight() {
        return weight;
    }
}
