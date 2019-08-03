package series.serie3.planningsubwaytrip.model;

public class Line {

    public String name;
    public Edge edge;
    public int weight;
    public Line(String theName, Edge theEdge){
        name= theName;
        edge=theEdge;
    }

    public String getName() {
        return name;
    }

    public void setWeight(int w) {
        weight =w;
    }

    public Edge getEdge() {
        return edge;
    }
}
