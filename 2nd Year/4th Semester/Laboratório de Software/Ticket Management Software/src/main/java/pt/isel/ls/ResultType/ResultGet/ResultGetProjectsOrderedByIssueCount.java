package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetProjectsOrderedByIssueCount implements Result {

    private List<Project> projectsOrderedByIssueCount;

    public ResultGetProjectsOrderedByIssueCount(List<Project> projectsOrderedByIssueCount) {
        this.projectsOrderedByIssueCount = projectsOrderedByIssueCount;
    }

    public List<Project> getValue() {
        return projectsOrderedByIssueCount;
    }
}