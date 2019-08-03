package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetProjectsOrderedByOpenIssueCount implements Result {
    private List<Project> projectsOrderedByOpenIssueCount;

    public ResultGetProjectsOrderedByOpenIssueCount(List<Project> projectsOrderedByOpenIssueCount) {
        this.projectsOrderedByOpenIssueCount = projectsOrderedByOpenIssueCount;
    }

    public List<Project> getValue() {
        return projectsOrderedByOpenIssueCount;
    }
}
