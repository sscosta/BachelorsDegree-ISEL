package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetProjectsOrderedByClosedIssueCount implements Result {
    private List<Project> projectsOrderedByOpenIssueCount;

    public ResultGetProjectsOrderedByClosedIssueCount(List<Project> projectsOrderedByOpenIssueCount) {

        this.projectsOrderedByOpenIssueCount = projectsOrderedByOpenIssueCount;
    }

    public List<Project> getValue() {
        return projectsOrderedByOpenIssueCount;
    }
}
