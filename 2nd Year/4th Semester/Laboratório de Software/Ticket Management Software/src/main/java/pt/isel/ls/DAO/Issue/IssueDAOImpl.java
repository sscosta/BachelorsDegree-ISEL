package pt.isel.ls.DAO.Issue;

import pt.isel.ls.DAO.IssueLabel.IssueLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Issue;

import java.sql.*;
import java.text.SimpleDateFormat;
import java.util.*;

public class IssueDAOImpl implements IssueDAO {
    private static final String STATE_KEY = "state";
    private final Connection connection;
    private static final String TABLE_NAME = "Issue";
    private static String QUERY = "SELECT Issue.id, Issue.name, Issue.description, Issue.Created, Issue.dateOfClosure, Issue.projectName, Issue.statusDesc, Issue.updated FROM Project\n" +
            "\tINNER JOIN Issue ON Project.name = Issue.projectName\n" +
            "\tWHERE Project.id = ? ";

    public IssueDAOImpl(Connection connection) {
        this.connection = connection;
    }

    @Override
    public List<Issue> getIssueList(int projectId, Map paramMap) throws InvalidParamException {

        if (paramMap.containsKey(STATE_KEY) && !paramMap.get(STATE_KEY).equals("all"))
            QUERY += "and Issue.statusDesc= '" + paramMap.get(STATE_KEY) + "' ";

        if (paramMap.get("direction") != null)
            QUERY += paramMap.get("direction");

        List<Issue> issuesList = new LinkedList<>();
        PreparedStatement preparedStatement;
        try {
            preparedStatement = connection.prepareStatement(QUERY);

            preparedStatement.setInt(1, projectId);
            ResultSet resultSet = preparedStatement.executeQuery();

            while (resultSet.next()) {
                int id = resultSet.getInt(1);
                String name = resultSet.getString(2);
                String description = resultSet.getString(3);
                java.sql.Timestamp creationDate = resultSet.getTimestamp(4);
                java.sql.Timestamp closedDate = resultSet.getTimestamp(5);
                String projectName = resultSet.getString(6);
                String statusDesc = resultSet.getString(7);
                java.sql.Timestamp update = resultSet.getTimestamp(8);

                Issue issue = new Issue.IssueBuilder().withID(id).withName(name).withDescription(description)
                        .withCreationDate(creationDate).withClosedDate(closedDate).withProjectName(projectName)
                        .withStatusDesc(statusDesc).withUpdated(update).build();
                issuesList.add(issue);
            }
            return issuesList;
        } catch (SQLException e) {
            throw new InvalidParamException("Invalid Project Id");
        }
    }

    @Override
    public int create(Issue issue) throws InvalidParamException {
        PreparedStatement preparedStatement;
        int createdIssueID;

        try {
            preparedStatement = connection.prepareStatement(
                    "Insert into " + TABLE_NAME + "(name,description,projectName,created)" +
                            " values (?,?,?,?)", Statement.RETURN_GENERATED_KEYS);
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy.MM.dd.HH.mm.ss");
            preparedStatement.setString(1, issue.name);
            preparedStatement.setString(2, issue.description);
            preparedStatement.setString(3, issue.projectName);
            preparedStatement.setTimestamp(4, new Timestamp(System.currentTimeMillis()));
            preparedStatement.executeUpdate();
            ResultSet resultSetToGetKeys = preparedStatement.getGeneratedKeys();
            resultSetToGetKeys.next();
            createdIssueID = resultSetToGetKeys.getInt(1);
            return createdIssueID;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to create new Issues " +
                    e.getMessage());
        }
    }

    @Override
    public void update(Issue i, String[] params) {
    }

    @Override
    public void delete(Issue i) {
    }

    @Override
    public Issue getSingleIssue(int id) throws CommandException {
        Issue issue = null;
        PreparedStatement preparedStatement;
        try {
            preparedStatement = connection.prepareStatement(
                    "select * from " + TABLE_NAME + " where id = ?");

            preparedStatement.setInt(1, id);

            ResultSet resultSet = preparedStatement.executeQuery();
            if (resultSet.next()) {
                String name = resultSet.getString(2);
                String description = resultSet.getString(3);
                Timestamp creationDate = resultSet.getTimestamp(4);
                Timestamp closedDate = resultSet.getTimestamp(5);
                String projectName = resultSet.getString(6);
                String statusDesc = resultSet.getString(7);
                Timestamp updated = resultSet.getTimestamp(8);

                issue = new Issue.IssueBuilder().withID(id).withName(name).withDescription(description)
                        .withCreationDate(creationDate).withClosedDate(closedDate).withProjectName(projectName)
                        .withStatusDesc(statusDesc).withUpdated(updated).build();
            }
            if (issue != null) {
                issue.setComments(getComments(id));
                issue.setLabels(new IssueLabelDAOImpl(connection).getLabels(id));
                return issue;
            }
            else
                throw new CommandException("Invalid Issue Id");
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get Issue " +
                    e.getMessage());
        }
    }

    @Override
    public void openIssue(Issue i) throws InvalidParamException {
        try {
            PreparedStatement preparedStatement = connection.prepareStatement(
                    "update " + TABLE_NAME + " set statusDesc='open', dateofclosure=NULL where projectName = ? and id=?;");
            preparedStatement.setString(1, i.projectName);
            preparedStatement.setInt(2, i.ID);
            preparedStatement.executeUpdate();

        } catch (SQLException e) {
            throw new InvalidParamException("Unable to open Issue " +
                    e.getMessage());
        }
    }

    @Override
    public void closeIssue(Issue i) throws InvalidParamException {
        try {
            String closeDate = String.valueOf(new Timestamp(System.currentTimeMillis()));
            PreparedStatement preparedStatement = connection.prepareStatement(
                    "update " + TABLE_NAME + " set statusDesc='closed', dateofclosure = '"+closeDate+"' where projectName = ? and id=?;");
            preparedStatement.setString(1, i.projectName);
            preparedStatement.setInt(2, i.ID);
            preparedStatement.executeUpdate();

        } catch (SQLException e) {
            throw new InvalidParamException("Unable to close Issue " +
                    e.getMessage());
        }
    }

    public LinkedList<Comment> getComments(int id) throws InvalidParamException {
        try {
            PreparedStatement preparedStatement = connection.prepareStatement("select * from Comment where issueID=?");
            preparedStatement.setInt(1, id);
            ResultSet resultSet = preparedStatement.executeQuery();
            LinkedList<Comment> commentList = new LinkedList<>();
            while (resultSet.next()) {
                int comId = resultSet.getInt(1);
                String proName = resultSet.getString(3);
                java.sql.Timestamp date = resultSet.getTimestamp(4);
                String text = resultSet.getString(5);
                commentList.add(new Comment.CommentBuilder().withId(comId).withIssueID(id)
                        .withProjectName(proName).withDate(date).withText(text).build());
            }
            return commentList;

        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get comments " + e.getMessage());

        }
    }


    private final String ISSUES_FROM_PROJECT_WITH_LABEL = "select Issue.id, Issue.name, Issue.description," +
            " Issue.created, Issue.dateOfClosure, Issue.projectName," +
            " statusDesc, updated from" +
            " (select * from Project where Project.id= ? ) as A" +
            " inner join Issue" +
            " on A.name = Issue.ProjectName" +
            " inner join IssueLabel" +
            " on IssueLabel.IssueID=Issue.id and IssueLabel.projectName=Issue.projectName" +
            " where labelDesc = ?";

    @Override
    public List<Issue> getIssuesFromProjectWithLabel(int projectID, String labelDesc) throws InvalidParamException {
        List<Issue> issues = new LinkedList<>();
        try {
            PreparedStatement preparedStatement = connection.prepareStatement(ISSUES_FROM_PROJECT_WITH_LABEL);
            preparedStatement.setInt(1, projectID);
            preparedStatement.setString(2, labelDesc);
            ResultSet resultSet = preparedStatement.executeQuery();
            while (resultSet.next()) {
                int issueID = resultSet.getInt(1);
                String issueName = resultSet.getString(2);
                String issueDesc = resultSet.getString(3);
                java.sql.Timestamp created = resultSet.getTimestamp(4);
                java.sql.Timestamp closure = resultSet.getTimestamp(5);
                String projectName = resultSet.getString(6);
                String statusDesc = resultSet.getString(7);
                java.sql.Timestamp updated = resultSet.getTimestamp(8);
                Issue issue = new Issue.IssueBuilder()
                        .withID(issueID)
                        .withName(issueName)
                        .withDescription(issueDesc)
                        .withCreationDate(created)
                        .withClosedDate(closure)
                        .withProjectName(projectName)
                        .withStatusDesc(statusDesc)
                        .withUpdated(updated)
                        .build();
                issues.add(issue);
            }
            return issues;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get labels " + e.getMessage());
        }
    }

    final String IS_OPEN_QUERY_STRING = "select * from Issue where id=? and statusDesc='open'";
    @Override
    public boolean isOpen(int issueID) throws SQLException {
        PreparedStatement ps = connection.prepareStatement(IS_OPEN_QUERY_STRING);
        ps.setInt(1,issueID);
        ResultSet rs = ps.executeQuery();
        return rs.next();
    }
}