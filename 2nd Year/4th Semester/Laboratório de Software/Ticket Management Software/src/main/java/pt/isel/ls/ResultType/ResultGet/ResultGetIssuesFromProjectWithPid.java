package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Issue;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetIssuesFromProjectWithPid implements Result {

    private List<Issue> issues;

    public ResultGetIssuesFromProjectWithPid(List<Issue> issues) {
        this.issues = issues;
    }

    public List<Issue> getValue() {
        return issues;
    }
}

