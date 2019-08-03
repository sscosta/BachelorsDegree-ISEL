package pt.isel.ls.Controller;

import java.util.List;
import java.util.Map;

public class RequestContext {

    Parser parser;
    List<Pair<String,String>> labelList;

    public RequestContext(Parser parser) {
        this.parser = parser;
        labelList = (List<Pair<String,String>>) parser.getParamMap().get("labels");
    }

    public Map getParamMap() {
        return parser.getParamMap();
    }

    public boolean hasLabels() {
        return labelList!=null && !labelList.isEmpty();
    }

    public List<Pair<String,String>> getLabels() {
        return labelList;
    }

    public String getMethod() {
        return parser.getMethod();
    }

    public String getPath() {
        return parser.getPath();
    }

    public List<MethodPath> getMethodPathList() {
        return Router.getMethodPaths();
    }
}
