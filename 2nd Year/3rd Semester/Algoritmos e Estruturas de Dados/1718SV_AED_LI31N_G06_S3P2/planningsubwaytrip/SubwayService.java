package series.serie3.planningsubwaytrip;

import series.serie3.planningsubwaytrip.model.Edge;
import series.serie3.planningsubwaytrip.model.Line;
import series.serie3.planningsubwaytrip.model.Pair;
import series.serie3.planningsubwaytrip.model.Station;

import java.util.*;

public class SubwayService {
    public static String                   DESTINATION="";
    public static Pair<String,Station>[] graph;

    public static LinkedList<LinkedList<Station>> paths = new LinkedList<>();

    protected static int                    N; // dimension of graph

    public SubwayService(){
        graph = Loader.getGraph();
        N = graph.length;

    }
    public void allPathsFromTo( LinkedList<Station> visited) {
        LinkedList<Station> stations = visited.getLast().getAdjacent();
        // examine adjacent nodes
        for (Station station : stations) {
            if (visited.contains(station)) {
                continue;
            }
            if (station.getNome().equals(DESTINATION)) {
                visited.add(station);
                printPath(visited);
                visited.removeLast();
                break;
            }
        }
        for (Station station : stations) {
            if (visited.contains(station) || station.getNome().equals(DESTINATION)) {
                continue;
            }
            visited.addLast(station);
            allPathsFromTo(visited);
            visited.removeLast();
        }
    }
    private void printPath(LinkedList<Station> visited) {
        for (Station st : visited) {
            System.out.print(st);
            System.out.print(" ");
        }
        System.out.println();
    }



    // Funtion that implements Dijkstra's single source shortest path
    // algorithm for a graph represented using adjacency list
    // representation
    public void dijkstra ( Station src){
        src.setDistance(src.line[0].weight);

        List<Station> settledNodes   = new LinkedList<>();
        List<Station> unsettledNodes = new LinkedList<>();

            unsettledNodes.add(src);

            while (unsettledNodes.size() != 0) {
                Station currentNode = getLowestDistanceNode(unsettledNodes);
                unsettledNodes.remove(currentNode);
                Edge e = currentNode.adjList;
                while(e!=null){
                    Station adjacentNode = ((Station)((Pair<String,Station>)e.connectsTo).getValue());
                    Integer edgeWeight =e.weight + swapLinesWeight(currentNode,adjacentNode);
                    if(!settledNodes.contains(adjacentNode)){
                        calculateMinimumDistance(adjacentNode, edgeWeight, currentNode);
                        unsettledNodes.add(adjacentNode);
                    }
                    e= e.next;
                }
                settledNodes.add(currentNode);
            }
            //print Path
            Station dst = ServiceUtilities.fetchStationFromNameOfStation(graph,DESTINATION);
            printShortestPath(dst);
        }

    private void printShortestPath(Station dst) {
        if(dst==null)return;
        printShortestPath(dst.predecessor);
        System.out.println(dst);
    }


    private static Station getLowestDistanceNode(List < Station > unsettledNodes) {
        Station lowestDistanceNode = null;
        int lowestDistance = Integer.MAX_VALUE;
        for (Station node: unsettledNodes) {
            int nodeDistance = node.distance;
            if (nodeDistance < lowestDistance) {
                lowestDistance = nodeDistance;
                lowestDistanceNode = node;
            }
        }
        return lowestDistanceNode;
    }


    private static void calculateMinimumDistance(Station evaluationNode,
                                                 Integer edgeWeigh, Station sourceNode) {
        Integer sourceDistance = sourceNode.distance;
        if (sourceDistance + edgeWeigh < evaluationNode.distance) {
            evaluationNode.setDistance(sourceDistance + edgeWeigh);
            evaluationNode.predecessor = sourceNode;
            //DEBUG
            //System.out.println(evaluationNode);
        }
    }

    // The concept of having to swap lines envolves predecessor of current, current and adjacent.
    // Whenever current has more than one line and predecessor of current line is different from adjacent line
    //there was a line swap. the time to be incremented is the avg waiting time of the new line and the swapping time.
    private int swapLinesWeight(Station currentNode, Station adjacentNode) {
        //find the swappedLine
        Line[] linesFrst = currentNode.line;
        Line[] linesScnd = adjacentNode.line;
        for(int i=0;i<linesFrst.length;++i){
            for(int j=0;j<linesScnd.length;++j){
                Edge e= linesFrst[i].edge;
                while(e!=null){
                    if(e.connectsTo.equals(linesScnd[j]))//return waiting time + swapping time
                        return e.getWeight()+linesScnd[j].weight;
                    e=e.next;
                }
            }
        }
        return 0;
    }

    public void lessChanges(Station station) {
        station.nHops=0;

        Set<Station> settledNodes = new HashSet<>();
        Set<Station> unsettledNodes = new HashSet<>();

            unsettledNodes.add(station);

            while (unsettledNodes.size() != 0) {
                Station currentNode = getMinHopsNode(unsettledNodes);
                unsettledNodes.remove(currentNode);
                Edge e = currentNode.adjList;
                while(e!=null){
                    Station adjacentNode = ((Station)((Pair<String,Station>)e.connectsTo).getValue());
                    Integer hopNotHop = ServiceUtilities.isInTheSameLine(currentNode.predecessor,adjacentNode)?0:1;
                    //System.out.println(adjacentNode);
                    if (!settledNodes.contains(adjacentNode)) {
                        calculateMinimumHopDistance(adjacentNode, hopNotHop, currentNode);
                        unsettledNodes.add(adjacentNode);
                    }
                    e=e.next;
                }
                settledNodes.add(currentNode);
            }
        //print Path
        Station dst = ServiceUtilities.fetchStationFromNameOfStation(graph,DESTINATION);
        printShortestPath(dst);
    }

    private static Station getMinHopsNode(Set < Station > unsettledNodes) {
        Station lowestDistanceNode = null;
        int lowestHopCount = Integer.MAX_VALUE;
        for (Station node: unsettledNodes) {
            int nodeHops = node.nHops;
            if (nodeHops < lowestHopCount) {
                lowestHopCount = nodeHops;
                lowestDistanceNode = node;
            }
        }
        return lowestDistanceNode;
    }
    private static void calculateMinimumHopDistance(Station evaluationNode,int hopNotHop,Station sourceNode){
        Integer sourceHops = sourceNode.nHops;
        if (sourceHops + hopNotHop < evaluationNode.nHops) {
            evaluationNode.nHops = sourceHops + hopNotHop;
            evaluationNode.predecessor = sourceNode;
        }
    }
}
