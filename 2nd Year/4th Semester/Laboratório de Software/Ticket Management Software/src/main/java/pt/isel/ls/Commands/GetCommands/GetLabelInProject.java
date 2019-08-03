package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAO;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetLabelDetail;

import java.sql.Connection;
import java.util.Map;

public class GetLabelInProject implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        int projectID = (int) paramMap.get("projectID");
        String labelDesc = (String) paramMap.get("label-name");

        Connection connection = connectionProvider.getConnection();

        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject(projectID).name;

        ProjectLabelDAO labelDAO = new ProjectLabelDAOImpl(connection);
        ProjectLabel projectLabel = labelDAO.getLabelDetail(labelDesc,projectName);

        if(projectLabel==null)
            throw new CommandException("No such label");
        return new ResultGetLabelDetail(projectID,projectLabel);
    }
}
