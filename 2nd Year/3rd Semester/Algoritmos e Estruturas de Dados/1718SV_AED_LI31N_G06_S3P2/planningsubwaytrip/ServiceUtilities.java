package series.serie3.planningsubwaytrip;

import org.apache.commons.lang3.StringUtils;
import series.serie3.planningsubwaytrip.model.Line;
import series.serie3.planningsubwaytrip.model.Pair;
import series.serie3.planningsubwaytrip.model.Station;

public class ServiceUtilities {

    public static Station fetchStationFromNameOfStation(Pair<String,Station> graph[] , String stationName){
        int                  idx = index(stationName);
        Pair<String,Station> p   = graph[idx];
        while(p!=null && !p.getKey().equals(stationName))
            p=p.getNext();
        if(p==null){
            System.out.println("A estação não é válida");
            return null;
        }
        return p.getValue();
    }

    public static String fetchNameOfStationFromLine(String line) {
        String [] str = line.split(":");
        //System.out.println(str[0]);
        //remover tabs
        str[0] = str[0].replace("\t", "");
        return str[0];
    }

    public static Line[] fetchLinesFromString(String line){
        String [] str = line.split(":");
        str = StringUtils.split(str[1]);
        Line [] theLines = new Line[str.length];
        for(int i=0;i<str.length;++i){
            //falta remover tabs de str[i]
            str[i] = str[i].replace("\t", "");
            theLines[i]= new Line(str[i],null);
            //System.out.println(str[i]);
        }
        return theLines;
    }
    protected static int index(Object k){
        int hc = k.hashCode();
        int m = hc%SubwayService.N;
        return m<0?m+SubwayService.N:m;
    }

    public static boolean isInTheSameLine(Station first, Station second) {
        if(first==null || second == null) return true;
            Line[] linesFrst = first.line;
            Line[] linesScnd = second.line;
            for (int i = 0; i < linesFrst.length; ++i) {
                for (int j = 0; j < linesScnd.length; ++j) {
                    if (linesFrst[i].name.equals(linesScnd[j].name))
                        return true;
                }
            }
            return false;
    }
}
