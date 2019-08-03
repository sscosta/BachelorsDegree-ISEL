package pt.isel.ls.DAO.Comment;

import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Comment;
import java.util.LinkedList;

public interface CommentDAO {

    LinkedList<Comment> getAll(int pid, int issueID) throws InvalidParamException;

    int create(Comment c) throws  InvalidParamException;

    void update(Comment c, String[] params);

    void delete(Comment c);

    Comment getSingleComment(int projectID,int issueId,int commentId) throws CommandException;
}
