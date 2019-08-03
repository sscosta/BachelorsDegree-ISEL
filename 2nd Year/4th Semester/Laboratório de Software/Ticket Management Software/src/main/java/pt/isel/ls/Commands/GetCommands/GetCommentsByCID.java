package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Comment.CommentDAO;
import pt.isel.ls.DAO.Comment.CommentDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetComments;

import java.sql.Connection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class GetCommentsByCID implements Command {
    private static final String ISSUE_ID_KEY   = "issueID";
    private static final String PROJECT_ID_KEY = "projectID";
    private static final String COMMENT_ID_KEY = "commentID";

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();
        int pid = (int) paramMap.get(PROJECT_ID_KEY);
        int issueID = (int) paramMap.get(ISSUE_ID_KEY);
        int cid = (int) paramMap.get(COMMENT_ID_KEY);
        CommentDAO commentDAO = new CommentDAOImpl(connection);
        Comment comment = commentDAO.getSingleComment(pid, issueID, cid);

        LinkedList<Comment> commentList = commentDAO.getAll(pid,issueID);

        return new ResultGetComments(comment, commentList);
    }
}
