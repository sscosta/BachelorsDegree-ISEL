package pt.isel.ls.ResultType;

import pt.isel.ls.Model.Issue;

public class ResultUpdateIssue implements Result {

    private CustomWrapper wrapper;

    public ResultUpdateIssue(Issue issue, int pid) {
        wrapper = new CustomWrapper(issue, pid);
    }


    @Override
    public Object getValue() {
        return wrapper;
    }
}
