package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Issue.IssueDAO;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.DAO.Label.LabelDAO;
import pt.isel.ls.DAO.Label.LabelDAOImpl;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Label;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssuesWithLabel;

import java.sql.Connection;
import java.util.List;
import java.util.Map;

public class GetIssuesWithLabel implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        final String LABEL_NAME_KEY = "label-name";
        final String PROJECT_ID_KEY = "projectID";

        int projectID = (int) paramMap.get(PROJECT_ID_KEY);
        String labelDesc = (String) paramMap.get(LABEL_NAME_KEY);

        Connection conn = connectionProvider.getConnection();

        ProjectDAO projectDAO = new ProjectDAOImpl(conn);
        String projectName = projectDAO.getSingleProject(projectID).name;

        IssueDAO issueDAO  = new IssueDAOImpl(conn);
        List<Issue> issueList = issueDAO.getIssuesFromProjectWithLabel(projectID,labelDesc);

        LabelDAO labelDAO  = new LabelDAOImpl(conn);
        Label label = labelDAO.getLabelWithName(labelDesc,projectName);
        if(label==null)
            throw new CommandException("No such label");
        return new ResultGetIssuesWithLabel(issueList,label,projectID);
    }
}
