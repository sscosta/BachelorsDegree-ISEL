package pt.isel.ls.DAO.Comment;

import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Comment;

import java.sql.*;
import java.util.LinkedList;


public class CommentDAOImpl implements CommentDAO {

    private final Connection connection;

    public CommentDAOImpl(Connection connection) {
        this.connection = connection;
    }

    @Override
    public LinkedList<Comment> getAll(int pid, int issueID) throws InvalidParamException {
        String query = "select * from Comment\n" +
                "inner join Project on Project.name = Comment.projectName\n" +
                "where Comment.issueID = ? and Project.id = ?";

        try {
            PreparedStatement preparedStatement = connection.prepareStatement(query);
            preparedStatement.setInt(1, issueID);
            preparedStatement.setInt(2, pid);
            ResultSet resultSet = preparedStatement.executeQuery();
            LinkedList<Comment> commentList = new LinkedList<>();
            while (resultSet.next()) {
                int comId = resultSet.getInt(1);
                String proName = resultSet.getString(3);
                java.sql.Timestamp date = resultSet.getTimestamp(4);
                String text = resultSet.getString(5);
                commentList.add(new Comment.CommentBuilder().withId(comId).withIssueID(issueID)
                        .withProjectName(proName).withDate(date).withText(text).build());
            }
            return commentList;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get comments " +
                    e.getMessage());
        }
    }

    @Override
    public int create(Comment c) throws InvalidParamException {
        PreparedStatement preparedStatement;
        int createdCommentID;
        try {
            preparedStatement = connection.prepareStatement(
                    "insert into Comment (issueID,projectName,text, date) values(?,?,?,?);",
                    Statement.RETURN_GENERATED_KEYS);
            preparedStatement.setInt(1, c.issueID);
            preparedStatement.setString(2, c.projectName);
            preparedStatement.setString(3, c.text);
            preparedStatement.setTimestamp(4, new Timestamp(System.currentTimeMillis()));
            preparedStatement.executeUpdate();
            ResultSet resultSetToGetKeys = preparedStatement.getGeneratedKeys();
            resultSetToGetKeys.next();
            createdCommentID = resultSetToGetKeys.getInt(1);

            return createdCommentID;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to create comment " +
                    e.getMessage());
        }
    }

    @Override
    public void update(Comment c, String[] params) {

    }

    @Override
    public void delete(Comment c) {

    }

    @Override
    public Comment getSingleComment(int pid, int issueID, int cid) throws CommandException {
        String query = "select * from Comment\n" +
                "inner join Project on Project.name = Comment.projectName\n" +
                "where Comment.issueID = ? and Project.id = ? and Comment.id = ?";
        PreparedStatement preparedStatement = null;
        try {
            preparedStatement = connection.prepareStatement(query);

            preparedStatement.setInt(1, issueID);
            preparedStatement.setInt(2, pid);
            preparedStatement.setInt(3, cid);
            ResultSet resultSet = preparedStatement.executeQuery();
            Comment comment = null;
            if (resultSet.next()) {
                int comId = resultSet.getInt(1);
                String proName = resultSet.getString(3);
                java.sql.Timestamp date = resultSet.getTimestamp(4);
                String text = resultSet.getString(5);
                comment = new Comment.CommentBuilder().withId(comId).withIssueID(issueID)
                        .withProjectName(proName).withDate(date).withText(text).build();
            }
            if (comment == null)
                throw new CommandException("Invalid Comment Id");

            return comment;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get comment " +
                    e.getMessage());
        }
    }
}
