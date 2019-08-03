package series.serie3.planningsubwaytrip;

import series.serie3.planningsubwaytrip.console.presentationlayer.Console;
import series.serie3.planningsubwaytrip.model.Pair;
import series.serie3.planningsubwaytrip.model.Station;

import java.util.List;



/*Planning planningsubwaytrip trip app
 *
 * authors: mcirja & sscosta
 * */

public class planningsubwaytrip {

    public static void main(String[] args) {


        //To begin the execution of the app, the command java planningsubwaytrip linhas.txt estacoes.txt has to be executed.
        // where

        Loader l = new Loader(args);
        try {
            (new Console()).run();
        } catch (Exception e) {
            System.out.println("Sorry something silly happened!!!");
            System.out.println("Cause: " + e.getMessage());
            System.out.println("Program aborted.");
            e.printStackTrace();
        }



	    //DEBUG: All lines in
        //BFS("Oriente");

       // LinkedList<Station> visited = new LinkedList<>();
        //visited.add(getStationFromString(START));
        //new planningsubwaytrip().allPathsFromTo(graph,visited);
    }



    private static <E> void printResult(List<E> allEs) {
        allEs.stream().forEach((aGeneric) -> System.out.println(aGeneric));
    }
    private static int getWeightFromString(String s) {
        String [] parser = s.split(":");
        return (Integer.valueOf(parser[0])*60) + Integer.valueOf(parser[1]);
    }







}
