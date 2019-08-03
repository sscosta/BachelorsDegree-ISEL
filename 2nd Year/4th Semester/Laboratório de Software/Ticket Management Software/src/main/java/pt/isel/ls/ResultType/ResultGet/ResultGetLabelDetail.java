package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

public class ResultGetLabelDetail implements Result {
    private CustomWrapper wrapper;

    public ResultGetLabelDetail(int projectID, ProjectLabel label) {
        wrapper = new CustomWrapper(label, projectID);
    }

    @Override
    public Object getValue() {
        return wrapper;
    }
}
