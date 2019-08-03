package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssues;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.Map;

public class GetIssuesOrderedByCommentCount implements Command {

    private static String ORDER_ISSUES_BY_COMMENT_QUERY = "select Issue.id,Issue.name,Issue.description," +
            "Issue.created,Issue.dateOfClosure,Issue.projectName," +
            "Issue.statusDesc,Issue.updated from " +
            "(select Issue.projectName,Issue.id, count(*) as nComments from Issue inner join " +
            "Comment on Issue.projectName=Comment.projectName and Issue.id=Comment.issueID " +
            "group by Issue.projectName,Issue.id ) as A inner join " +
            "Issue inner join Project on Issue.projectName=Project.name on " +
            "Issue.projectName=A.projectName and Issue.id=A.id where Project.id=?";
    private static final String SECOND_PART = " order by nComments ";

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        String query = ORDER_ISSUES_BY_COMMENT_QUERY;
        Connection connection = con.getConnection();
        LinkedList<Issue> issuesOrderedByComment = new LinkedList<>();
        if (paramMap.containsKey("state") && !paramMap.get("state").equals("all"))
            query += " and Issue.statusDesc = '" + paramMap.get("state") + "'";
        query += SECOND_PART;
        query += paramMap.containsKey("direction") ? paramMap.get("direction") : "";

        try {
            PreparedStatement ps = connection.prepareStatement(query);
            int projectID = (int) paramMap.get("projectID");
            ps.setInt(1, projectID);
            ResultSet resultSet = ps.executeQuery();
            while (resultSet.next()) {
                int issueID = resultSet.getInt(1);
                String issueName = resultSet.getString(2);
                String descr = resultSet.getString(3);
                java.sql.Timestamp created = resultSet.getTimestamp(4);
                java.sql.Timestamp closeDate = resultSet.getTimestamp(5);
                String projName = resultSet.getString(6);
                String status = resultSet.getString(7);
                java.sql.Timestamp updated = resultSet.getTimestamp(8);

                issuesOrderedByComment.add(new Issue.IssueBuilder().withID(issueID)
                        .withName(issueName)
                        .withDescription(descr)
                        .withCreationDate(created)
                        .withClosedDate(closeDate)
                        .withProjectName(projName)
                        .withStatusDesc(status)
                        .withUpdated(updated).build());
            }
            return new ResultGetIssues(issuesOrderedByComment,projectID);
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get project ordered by comment count " +
                    e.getMessage());
        }
    }
}
