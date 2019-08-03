package pt.isel.ls.DAO.Project;

import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Project;

import java.util.List;

public interface ProjectDAO  {
    List<Project> getAll() throws InvalidParamException;

    long create(Project p) throws InvalidParamException;

    void update(Project p, String[] params);

    void delete(Project p);

    Project getSingleProject(int id) throws CommandException;

}
