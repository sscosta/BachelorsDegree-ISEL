package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetProjectsOrderedByCreationDate implements Result {
    private List<Project> projectsOrderedByCreationDate;

    public ResultGetProjectsOrderedByCreationDate(List<Project> projectsOrderedByCreationDate) {
        this.projectsOrderedByCreationDate = projectsOrderedByCreationDate;
    }

    public List<Project> getValue() {
        return projectsOrderedByCreationDate;
    }
}
