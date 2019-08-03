package pt.isel.ls.Commands.PostCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAO;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAOImpl;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Exceptions.LabelNotAllowedInProjectException;
import pt.isel.ls.Model.IssueLabel;
import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetLabelsInIssue;
import pt.isel.ls.ResultType.ResultPost.ResultPostIssueLabel;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class PostLabelInIssue implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        final String LABELS_KEY = "label";
        final String PROJECT_ID_KEY = "projectID";
        final String ISSUE_ID_KEY = "issueID";

        Connection connection = connectionProvider.getConnection();
        Map paramMap = requestContext.getParamMap();
        String label = (String) paramMap.get(LABELS_KEY);
        int projectID = (int) paramMap.get(PROJECT_ID_KEY);
        int issueID = (int) paramMap.get(ISSUE_ID_KEY);

        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject((int) paramMap.get(PROJECT_ID_KEY)).name;
        checkLabelInProject(connection, label, projectName);
        IssueLabelDAO issueLabelDAO = new IssueLabelDAOImpl(connection);
        try {

            issueLabelDAO.addLabelToIssue(
                    new IssueLabel
                            .IssueLabelBuilder()
                            .withProjectName(projectName)
                            .withIssueID(issueID)
                            .withLabelDesc(label)
                            .build());

            LinkedList<ProjectLabel> projectLabelList = new ProjectLabelDAOImpl(connection).getLabelsForProject(projectName);
            LinkedList<Label> labelList = issueLabelDAO.getLabels(issueID);

            return new ResultPostIssueLabel(projectID, issueID, projectLabelList, labelList);
        } catch (LabelNotAllowedInProjectException e) {
            throw new InvalidParamException("Unable to post label: " + e.getMessage());
        }
    }

    private boolean checkLabelInProject(Connection connection, String label, String projectName) throws CommandException {
        final String QUERY = "select projectName , labelDesc from ProjectLabel where projectName = ? and labelDesc= ?";
        try {
            PreparedStatement ps = connection.prepareStatement(QUERY);
            ps.setString(1, projectName);
            ps.setString(2, label);

            ResultSet rs = ps.executeQuery();
            if (rs.next()) {
                String projectNameFound = rs.getString(1);
                String labelDescriptionFound = rs.getString(2);
                return labelDescriptionFound.equals(label) && projectNameFound.equals(projectName);
            }
            throw new CommandException("Label doesn't exist in project " + projectName);
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get Label Issue " +
                    e.getMessage());
        }

    }
}
