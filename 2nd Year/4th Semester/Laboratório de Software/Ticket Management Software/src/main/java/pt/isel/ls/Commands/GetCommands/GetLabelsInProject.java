package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAO;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAOImpl;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetLabelsInProject;

import java.sql.Connection;
import java.util.LinkedList;
import java.util.Map;

public class GetLabelsInProject implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        int projectID = (int) paramMap.get("projectID");

        Connection connection = connectionProvider.getConnection();
        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject(projectID).name;
        ProjectLabelDAO projectLabelDAO = new ProjectLabelDAOImpl(connection);
        LinkedList<ProjectLabel> labelList = projectLabelDAO.getLabelsForProject(projectName);
        return new ResultGetLabelsInProject(labelList,projectID);
    }
}
