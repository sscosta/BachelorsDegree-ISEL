package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Issue;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

public class ResultGetIssueById implements Result {

    private CustomWrapper wrapper;

    public ResultGetIssueById(Issue issue, int id) {
        wrapper = new CustomWrapper(issue, id);
    }

    public Object getValue() {
        return wrapper;
    }

}
