package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;

public class ResultGetProjectWithId implements Result {
    private Project project;

    public ResultGetProjectWithId(Project project) {
        this.project = project;
    }

    public Project getValue() {
        return project;
    }
}
