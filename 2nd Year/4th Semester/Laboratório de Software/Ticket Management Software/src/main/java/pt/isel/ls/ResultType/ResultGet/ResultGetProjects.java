package pt.isel.ls.ResultType.ResultGet;

import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;

import java.util.List;

public class ResultGetProjects implements Result<List<Project>> {
    private String name;
    List<Project> projects;

    public ResultGetProjects(List<Project> projectsList) {
       this.projects = projectsList;
    }

    public ResultGetProjects(List<Project> projects, String name) {
        this.projects = projects;
        this.name = name;
    }

    public List<Project> getValue() {
        return projects;
    }
    public String getProjectName(){return name;}
}
