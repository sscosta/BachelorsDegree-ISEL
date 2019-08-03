package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Comment.CommentDAO;
import pt.isel.ls.DAO.Comment.CommentDAOImpl;
import pt.isel.ls.DAO.Issue.IssueDAO;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetComments;
import java.sql.Connection;
import java.util.LinkedList;
import java.util.Map;

public class GetComments implements Command {
    private static final String ISSUE_ID_KEY   = "issueID";
    private static final String PROJECT_ID_KEY = "projectID";
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        int projID = (int) paramMap.get(PROJECT_ID_KEY);
        int issueID = (int) paramMap.get(ISSUE_ID_KEY);

        Connection connection = connectionProvider.getConnection();
        LinkedList<Comment> comments;
        if(issueIsInProject(issueID,projID,connection)) {
            CommentDAO commentDAO = new CommentDAOImpl(connection);
            comments = commentDAO.getAll(projID,issueID);
            }
        else
            throw new CommandException("Issue Id not found in This Project");

        return new ResultGetComments(comments.getFirst(),comments);
    }

    private boolean issueIsInProject(int issueID, int projID, Connection connection) throws CommandException {
        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        Project project = projectDAO.getSingleProject(projID);
        IssueDAO issueDAO = new IssueDAOImpl(connection);
        Issue issue = issueDAO.getSingleIssue(issueID);
        return project.name.equals(issue.projectName);
    }
}
