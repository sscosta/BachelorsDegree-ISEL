package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.Pair;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Exceptions.SQLConnectionException;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssues;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class GetIssuesOrderedByDate implements Command {
    private String INIT_QUERY_LABEL = "select id, name, description, created, dateOfClosure, Issue.projectName, statusDesc, updated\n" +
            "from Issue\n" +
            "inner join IssueLabel \n" +
            "\ton IssueLabel.projectName = Issue.projectName and IssueLabel.IssueID = issue.id\n" +
            "\tand Issue.projectName=?";
    private String INIT_SIMPLE_QUERY = "select id, name, description, created, dateOfClosure," +
            " Issue.projectName, statusDesc, updated from Issue where Issue.projectName=?";
    private StringBuilder query;

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        query = new StringBuilder();
        Connection connection = con.getConnection();
        Map paramMap = requestContext.getParamMap();

        int projectID = (int) paramMap.get("projectID");
        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject(projectID).name;
        List<Issue> issuesOrderedByDate = new LinkedList<>();
        PreparedStatement ps;
        try {
            if(hasLabels(paramMap)){
                query.append(INIT_QUERY_LABEL);
                int size = addLabels(paramMap);
                setFilterList(paramMap);
                havingLabels(size);
                setSort(paramMap);
                query.append(getDirection(paramMap));

                ps = connection.prepareStatement(query.toString());

                int column = 0;
                ps.setString(++column,projectName);
                for (int i = 0; i < size; i++) {
                    List<Pair<String, String>> labels = (List<Pair<String, String>>) paramMap.get("labels");
                    String lbl = labels.get(i).v;
                    ps.setString(++column,lbl);
                }
                ResultSet resultSet = ps.executeQuery();
                getResultList(issuesOrderedByDate, resultSet);
            }
            else{
                query.append(INIT_SIMPLE_QUERY);
                setFilterList(paramMap);
                setSort(paramMap);
                query.append(getDirection(paramMap));
                ps = connection.prepareStatement(query.toString());
                ps.setString(1,projectName);
                ResultSet resultSet = ps.executeQuery();
                getResultList(issuesOrderedByDate, resultSet);
            }

            ResultGetIssues res = new ResultGetIssues(issuesOrderedByDate, projectID);
            res.pid = projectID;
            return res;
        } catch (SQLException e) {
            throw new SQLConnectionException("Unable to query database");
        }
    }

    private void setSort(Map paramMap) throws InvalidParamException {
        if(paramMap.containsKey("sort")) {
            if(isValidSort(paramMap))
                query.append(" order by ").append(paramMap.get("sort")).append(" ");
        }
    }

    private boolean isValidSort(Map paramMap) throws InvalidParamException {
        String sort = (String) paramMap.get("sort");
        if(sort.equals("created") || sort.equals("updated"))
            return true;
        throw new InvalidParamException("sort value do not exists");
    }

    private void havingLabels(int size) {
        query.append("group by id, name, description, created, dateOfClosure, Issue.projectName, statusDesc, updated\n" +
                "\thaving count(IssueLabel.labelDesc) = ").append(size);
    }

    private int addLabels(Map paramMap) {
        List<Pair<String, String>> labels = (List<Pair<String, String>>) paramMap.get("labels");
        int size = labels.size();
        query.append(" and labelDesc in (");
        for (int i = 0; i < size; i++) {
            query.append("?");
            if(i==size-1)
                query.append(")");
            else
                query.append(",");
        }
        return size;
    }

    private void setFilterList(Map paramMap) throws InvalidParamException {
        String state = (String) paramMap.get("state");
        if(state==null || state.equals("all"))return;
        if (state.equals("open") || state.equals("closed")) {
            query.append(" and statusDesc = \'").append(state+"\' ");
            return;
        }
        throw new InvalidParamException(paramMap.get("state")+" is not valid");

    }

    private boolean hasLabels(Map paramMap) {
        return paramMap.containsKey("labels");
    }

    private String getDirection(Map paramMap) {
        return paramMap.containsKey("direction") ? String.valueOf(paramMap.get("direction")) : "";
    }

    private void getResultList(List<Issue> issuesOrderedByCreationDate, ResultSet resultSet) throws SQLException {
        while (resultSet.next()) {
            int id = resultSet.getInt(1);
            String name = resultSet.getString(2);
            String desc = resultSet.getString(3);
            java.sql.Timestamp creationDate = resultSet.getTimestamp(4);
            java.sql.Timestamp closedDate = resultSet.getTimestamp(5);
            String projName = resultSet.getString(6);
            String statusDesc = resultSet.getString(7);
            java.sql.Timestamp update = resultSet.getTimestamp(8);
            issuesOrderedByCreationDate.add(new Issue.IssueBuilder().withID(id).withName(name)
                    .withDescription(desc).withCreationDate(creationDate).withClosedDate(closedDate)
                    .withProjectName(projName).withStatusDesc(statusDesc).withUpdated(update).build());
        }
    }
}
