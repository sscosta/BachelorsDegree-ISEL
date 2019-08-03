package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Issue;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetIssues implements Result {

    private List<Issue> issues;
    public int pid;

    private String issueName;

    public ResultGetIssues(List<Issue> issuesOrdered, int projectID) {
        this.issues = issuesOrdered;
        this.pid = projectID;
    }

    public ResultGetIssues(List<Issue> issues, int projectID, String issueName) {
        this.issues = issues;
        this.issueName = issueName;
    }

    public String getIssueName() {
        return issueName;
    }

    public List<Issue> getValue() {
        return issues;
    }
}
