package pt.isel.ls.Commands.PostCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.DAO.Issue.IssueDAO;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.PresentationLayer.Console.Print;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.ResultType.ResultUpdateIssue;

import java.sql.Connection;
import java.util.Map;

public class OpenIssue implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();

        int projectID = (int) paramMap.get("projectID");
        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject(projectID).name;
        int issueID = (int) paramMap.get("issueID");

        IssueDAO issueDAO = new IssueDAOImpl(connection);
        Issue issue = new Issue.IssueBuilder()
                .withProjectName(projectName)
                .withID(issueID).build();
        issueDAO.openIssue(issue);
        issue = issueDAO.getSingleIssue(issueID);
        Print.message("Issue " + issueID + " in project " + projectName + " opened");
        return new ResultUpdateIssue(issue, projectID);
    }
}
