package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Label;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetIssuesWithLabel implements Result {

    public Label label;
    private List<Issue> issuesInProjectWithLabel;
    public int pid;

    public ResultGetIssuesWithLabel(List<Issue> issuesInProjectWithLabel, Label label, int projectID) {
        this.issuesInProjectWithLabel = issuesInProjectWithLabel;
        this.label = label;
        this.pid = projectID;
    }

    public List<Issue> getValue(){return issuesInProjectWithLabel;}
}
