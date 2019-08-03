package pt.isel.ls.Commands.PostCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Issue.IssueDAO;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.PresentationLayer.Console.Print;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssueById;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssues;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssuesFromProjectWithPid;
import pt.isel.ls.ResultType.ResultPost.ResultPostIssue;

import java.sql.Connection;
import java.util.List;
import java.util.Map;

public class PostIssue implements Command {

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();

        String ISSUE_NAME = "name";
        String issueName = (String) paramMap.get(ISSUE_NAME);
        String ISSUE_DESCRIPTION = "description";
        String description = (String) paramMap.get(ISSUE_DESCRIPTION);
        IssueDAO issueDAO = new IssueDAOImpl(connection);

        int pid= (int) paramMap.get("projectID");
        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject(pid).name;

        List<Issue> issues = issueDAO.getIssueList(pid,paramMap);
        for(Issue issue : issues)
            if(issue.projectName.equals(projectName) && issue.name.equals(issueName))
                return new ResultGetIssues(issues,pid,issueName);

        int iid = issueDAO.create(
                new Issue
                        .IssueBuilder()
                        .withName(issueName)
                        .withDescription(description)
                        .withProjectName(projectName)
                        .build()
        );
        if(iid==0)
            throw new CommandException("Could not create issue");
        return new ResultPostIssue(issueDAO.getSingleIssue(iid),pid);
    }
}