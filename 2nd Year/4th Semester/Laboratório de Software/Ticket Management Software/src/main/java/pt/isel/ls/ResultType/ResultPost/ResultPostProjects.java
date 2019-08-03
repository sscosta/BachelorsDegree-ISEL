package pt.isel.ls.ResultType.ResultPost;

import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;

public class ResultPostProjects implements Result {

    private Project project;

    public ResultPostProjects(Project project) {
        this.project = project;
    }

    @Override
    public Object getValue() {
        return project;
    }
}
