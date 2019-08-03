package pt.isel.ls.ResultType.ResultPost;

import pt.isel.ls.Model.Issue;
import pt.isel.ls.ResultType.CustomWrapper;
import pt.isel.ls.ResultType.Result;

public class ResultPostIssue implements Result {
    private CustomWrapper wrapper;

    public ResultPostIssue(Issue issue, int pid) {
        wrapper = new CustomWrapper(issue, pid);
    }

    @Override
    public Object getValue() {
        return wrapper;
    }
}
