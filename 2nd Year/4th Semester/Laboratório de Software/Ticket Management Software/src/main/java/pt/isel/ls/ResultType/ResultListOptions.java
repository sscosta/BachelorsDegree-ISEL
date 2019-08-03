package pt.isel.ls.ResultType;

import pt.isel.ls.Controller.MethodPath;

import java.util.List;

public class ResultListOptions implements Result {
    private List<MethodPath> methodPathList;
    public ResultListOptions(List<MethodPath> methodPathList) {
        this.methodPathList=methodPathList;
    }

    public List<MethodPath> getValue() {
        return methodPathList;
    }
}
