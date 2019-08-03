package series.serie3.planningsubwaytrip;

import series.serie3.planningsubwaytrip.model.Edge;
import series.serie3.planningsubwaytrip.model.Line;
import series.serie3.planningsubwaytrip.model.Pair;
import series.serie3.planningsubwaytrip.model.Station;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class Loader {
    private static int N;
    private static Pair<String,Station> [] graph;

    Loader(String[] args){
        loadStationsDataFromFile(args[1]);
        loadLineDataFromFile(args[0]);
    }
    static Pair<String,Station>[] getGraph(){
        return graph;
    }

    protected static void loadLineDataFromFile(String arg) {

        String lineParser[] , line; //auxiliary multipurpose variables

        try( BufferedReader brLines = new BufferedReader(new              //input file(s) name(s)
                InputStreamReader(planningsubwaytrip.class.getClassLoader().getResourceAsStream(arg)))){

            // The first line of the Lines File holds the number of lines in the network
            // the format of the line is as follows:
            //                  lineColor min.sec

            int nLines = Integer.valueOf(brLines.readLine());
            for(int j = 0;j<nLines;++j){
                line = brLines.readLine();
                lineParser = line.split(" ");
                String [] taux2 = lineParser[1].split(":");
                int weight = (Integer.parseInt(taux2[0])*60) + Integer.parseInt(taux2[1]);

                // After parsing correctly the line of the lines file, EVERY NODE in the graph THAT BELONGS
                // TO THE LINE must hold its waiting time
                for(int i = 0 ; i<N;++i){
                    Pair<String,Station> pIt = graph[i];
                    while(pIt!=null){
                        if(pIt.getValue().getLine().length==1 && pIt.getValue().getLine(0).getName().equals(lineParser[0]))
                            pIt.getValue().getLine(0).setWeight(weight);
                        pIt = pIt.getNext();
                    }
                }
            }

            while((line=brLines.readLine())!=null){
                //get parameters from line
                // format line1 line2 min.sec
                line = line.replace("\t","");
                lineParser = line.split(" ");
                String taux[] = lineParser[2].split(":");
                int weight = (Integer.valueOf(taux[0])*60) + Integer.valueOf(taux[1]);

                for(int i = 0;i<N;++i){
                    Pair<String,Station>pIt = graph[i];
                    //iterate index i of graph because there can be collisions and therefore more than one Pair in a bucket
                    while(pIt!=null){
                        //correct to list
                        if(pIt.getValue().getLine().length!=1) {
                            Line l1 = pIt.getValue().getLine(0);
                            Line l2 = pIt.getValue().getLine(1);
                            Edge e;
                            if(l1.getName().equals(lineParser[0])&& l2.getName().equals(lineParser[1])){
                                Edge e1 = new Edge(weight,pIt.getValue().getLine(1),l1.getEdge());
                                Edge e2 = new Edge(weight,pIt.getValue().getLine(0),l2.getEdge());
                                Edge tmp;
                                if(l1.getEdge()!=null) {
                                    tmp =l1.getEdge().getPrev();
                                    tmp = e1;
                                }
                                if(l2.getEdge()!=null) {
                                    tmp = l2.getEdge().getPrev();
                                    tmp = e2;
                                }
                                Edge head1;
                                head1 = l1.getEdge();
                                head1 = e1;
                                Edge head2;
                                head2=l2.getEdge();
                                head2 =e2;
                            } else if(l1.getName().equals(lineParser[1])&& l2.getName().equals(lineParser[0])) {
                                Edge e1 = new Edge(weight,pIt.getValue().getLine(0),l1.getEdge());
                                Edge e2 = new Edge(weight,pIt.getValue().getLine(1),l2.getEdge());
                                Edge prev1;
                                Edge prev2;
                                if(l1.getEdge()!=null) {
                                    prev1=l1.getEdge().getPrev();
                                    prev1 = e1;;
                                }
                                if(l2.getEdge()!=null) {
                                    prev2=l2.getEdge().getPrev();
                                    prev2 = e2;
                                }
                                Edge head1 = l1.getEdge();
                                head1 = e1;
                                Edge head2 = l2.getEdge();
                                head2= e2;
                            }
                        }
                        pIt=pIt.getNext();
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    protected static void loadStationsDataFromFile(String stFileName) {
        try(BufferedReader brStations = new BufferedReader(new              //input file(s) name(s)
                InputStreamReader(planningsubwaytrip.class.getClassLoader().getResourceAsStream(stFileName)))){
            //initialize graph
            N = Integer.valueOf(brStations.readLine());
            graph= (Pair<String,Station>[]) new Pair[N];
            String line;
            //read stations from file
            for(int i = 0; i < N ; i++){
                line = brStations.readLine();
                String name = ServiceUtilities.fetchNameOfStationFromLine(line);
                int idx = index(name);
                Pair<String,Station> newP = new Pair<String,Station>(name,new Station(i,name,ServiceUtilities.fetchLinesFromString(line)));
                if(graph[idx]==null)
                    graph[idx] = newP;
                else{
                    newP.next = graph[idx];
                    graph[idx]= newP;
                }
            }
            //format of line to process:
            // name : name - min:sec
            while((line=brStations.readLine())!=null){
                String [] nt = line.split("-");
                String [] nSt = nt[0].split(":");
                String [] taux = nt[1].replace(" ","").split(":");
                int time = (Integer.parseInt(taux[0])*60) + Integer.parseInt(taux[1]);

                //one int could be spared
                int idx1 =index(nSt[0]);
                int idx2 = index(nSt[1]);
                Pair<String,Station> st1 = graph[idx1];

                //mudar next em station para Pair ou colocar em pair um next
                while(st1!=null && !st1.getKey().equals(nSt[0]))
                    st1=st1.getNext();
                Pair<String,Station> st2 = graph[idx2];
                while(st2!=null && !st2.getKey().equals(nSt[1]))
                    st2=st2.getNext();

                Edge newEdg1 = new Edge(time,st2,st1.getValue().adjList);
                st1.getValue().adjList = newEdg1;

                Edge newEdg2 = new Edge(time,st1,st2.getValue().adjList);
                st2.getValue().adjList = newEdg2;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    protected static int index(Object k){
        int hc = k.hashCode();
        int m = hc%N;
        return m<0?m+N:m;
    }
}
