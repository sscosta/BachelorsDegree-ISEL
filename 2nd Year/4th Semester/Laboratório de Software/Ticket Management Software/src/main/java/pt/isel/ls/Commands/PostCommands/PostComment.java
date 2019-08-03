package pt.isel.ls.Commands.PostCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.DAO.Comment.CommentDAO;
import pt.isel.ls.DAO.Comment.CommentDAOImpl;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Issue.IssueDAO;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.ResultType.ResultPost.ResultPostComment;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.Map;

public class PostComment implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();

        String commentText = (String) paramMap.get("text");
        int projectId = (int) paramMap.get("projectID");
        ProjectDAO projectDAO  = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject(projectId).name;
        int issueID = (int) paramMap.get("issueID");

        CommentDAOImpl commentDAOImpl;
        commentDAOImpl = new CommentDAOImpl(connection);
        int id;
        try {
            if(issueIsOpen(issueID,connection)) {
                id = commentDAOImpl.create(new Comment.CommentBuilder()
                        .withIssueID(issueID)
                        .withProjectName(projectName)
                        .withText(commentText).build());
            }
            else
                throw new CommandException("No Comment created. Issue " + issueID + " is not open.");
        } catch (SQLException e) {
            throw new CommandException("Error while accessing database:" + e );
        }
        LinkedList<Comment> comments = commentDAOImpl.getAll(projectId, issueID);
        return new ResultPostComment(commentDAOImpl.getSingleComment(projectId,issueID,id), comments);
    }

    private boolean issueIsOpen(int issueID, Connection connection) throws SQLException {
        IssueDAO issueDAO = new IssueDAOImpl(connection);
        return issueDAO.isOpen(issueID);
    }
}
