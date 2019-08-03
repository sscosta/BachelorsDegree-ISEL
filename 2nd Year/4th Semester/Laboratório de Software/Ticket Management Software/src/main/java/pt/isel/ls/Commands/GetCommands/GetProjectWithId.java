package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAO;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Project;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.ResultType.ResultGet.ResultGetProjectWithId;

import java.sql.Connection;
import java.util.LinkedList;
import java.util.Map;

public class GetProjectWithId implements Command {
    private static final String PROJECT_ID_KEY = "projectID";

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();
        int PROJid = (int) paramMap.get(PROJECT_ID_KEY);

        ProjectDAOImpl projectDAOImpl;
        projectDAOImpl = new ProjectDAOImpl(connection);
        Project project;

        project = projectDAOImpl.getSingleProject(PROJid);
        if(project==null)
            throw new CommandException("Wrong ID");


        return new ResultGetProjectWithId(project);
    }
}
