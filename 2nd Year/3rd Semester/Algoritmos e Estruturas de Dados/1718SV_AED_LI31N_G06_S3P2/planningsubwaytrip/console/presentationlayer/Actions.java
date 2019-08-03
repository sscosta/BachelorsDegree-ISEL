package series.serie3.planningsubwaytrip.console.presentationlayer;

import series.serie3.planningsubwaytrip.ServiceUtilities;
import series.serie3.planningsubwaytrip.SubwayService;
import series.serie3.planningsubwaytrip.model.Station;

import java.util.LinkedList;
import java.util.Scanner;

public class Actions {
    private static String START="";

    //hashmap that keeps the pairs <nameOfStation,Station>  that comprise
    // the Lisbon metropolitan metro network
    //protected static Pair<String,Station>[] graph;
    //protected static int N; // dimension of graph
    private SubwayService subwayService;

    public SubwayService getSubwayService(){return subwayService;}

    public Actions(){
        /* Creates a new SubwayService but allows for future refactoring to support Dependency Injection */
        subwayService = new SubwayService();
    }

    public void allPaths(Scanner input, Console console)
    {
        START = Utilities.GetString(input, "Source?");
        subwayService.DESTINATION = Utilities.GetString(input,"Destination?");

        LinkedList<Station> visited   = new LinkedList<>();
        visited.add(ServiceUtilities.fetchStationFromNameOfStation(subwayService.graph,START));
        subwayService.allPathsFromTo(visited);
    }

    public void fastestPath(Scanner input, Console console)
    {
        START = Utilities.GetString(input,"Source?");
        subwayService.DESTINATION = Utilities.GetString(input,"Destination?");
        subwayService.dijkstra(ServiceUtilities.fetchStationFromNameOfStation(subwayService.graph,START));
    }

    public void pathWithLessChanges(Scanner input, Console console){
        START = Utilities.GetString(input,"Source?");
        subwayService.DESTINATION = Utilities.GetString(input,"Destination?");
        subwayService.lessChanges(ServiceUtilities.fetchStationFromNameOfStation(subwayService.graph,START));
    }

    public void Quit(Scanner input, Console console)
    {
        Boolean exit = Utilities.YesOrNoQuestion(input, "Are you sure?");
        if(exit)
        {
            System.out.println("Thank you for using the program! Bye!");
            console.Terminate();
        }
    }
}
