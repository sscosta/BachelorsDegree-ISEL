package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjects;

import java.sql.Connection;
import java.util.List;


public class GetProjects implements Command {

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Connection connection = con.getConnection();
        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        List<Project> projectsList = projectDAO.getAll();
        return new ResultGetProjects(projectsList);
    }
}
