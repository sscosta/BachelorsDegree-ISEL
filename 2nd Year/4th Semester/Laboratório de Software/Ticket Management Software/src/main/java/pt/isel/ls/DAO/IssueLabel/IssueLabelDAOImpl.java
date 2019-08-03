package pt.isel.ls.DAO.IssueLabel;

import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Exceptions.LabelNotAllowedInProjectException;
import pt.isel.ls.Model.IssueLabel;
import pt.isel.ls.Model.Label;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedList;

public class IssueLabelDAOImpl implements IssueLabelDAO {
    Connection connection;

    public IssueLabelDAOImpl(Connection connection) {
        this.connection = connection;
    }

    final String ADD_LABEL_TO_ISSUE_QUERY = "insert into IssueLabel (projectName,IssueID,labelDesc) values" +
            "(?,?,?);";

    @Override
    public boolean addLabelToIssue(IssueLabel issueLabel) throws LabelNotAllowedInProjectException{
        try {
            PreparedStatement ps = connection.prepareStatement(ADD_LABEL_TO_ISSUE_QUERY);
            ps.setString(1, issueLabel.projectName);
            ps.setInt(2, issueLabel.issueID);
            ps.setString(3, issueLabel.labelDesc);
            return ps.execute();

        } catch (SQLException e) {
            throw new LabelNotAllowedInProjectException("Label Not created in Project");
        }
    }

    final String DELETE_LABEL_FROM_ISSUE_QUERY = "delete from IssueLabel where projectName=? and IssueID=? and labelDesc=? ;";

    @Override
    public boolean deleteLabelFromIssue(IssueLabel issueLabel) throws InvalidParamException {
        try {
            PreparedStatement preparedStatement = connection.prepareStatement(DELETE_LABEL_FROM_ISSUE_QUERY);
            preparedStatement.setString(1, issueLabel.projectName);
            preparedStatement.setInt(2, issueLabel.issueID);
            preparedStatement.setString(3, issueLabel.labelDesc);
            return preparedStatement.execute();
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to delete label from issue " + e.getMessage());
        }
    }


    private static final String QUERY_TO_GET_LABELS = "select labelDesc from Issue inner join IssueLabel on Issue.id = IssueLabel.IssueID  and Issue.projectName=IssueLabel.projectName where Issue.id = ?";

    @Override
    public LinkedList<Label> getLabels(int id) throws InvalidParamException {
        LinkedList<Label> labels = new LinkedList<>();
        try {
            PreparedStatement ps = connection.prepareStatement(QUERY_TO_GET_LABELS);
            ps.setInt(1, id);
            ResultSet resultSet = ps.executeQuery();
            while (resultSet.next()) {
                Label label = new Label.LabelBuilder().withDescription(resultSet.getString(1)).build();
                labels.add(label);
            }
            return labels;
        } catch (SQLException e) {
            throw new InvalidParamException("Unable to get labels from issue " + e.getMessage());
        }
    }
}
