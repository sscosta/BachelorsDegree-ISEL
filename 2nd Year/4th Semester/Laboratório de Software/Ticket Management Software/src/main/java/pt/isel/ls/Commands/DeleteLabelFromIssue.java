package pt.isel.ls.Commands;

import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAO;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAOImpl;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.IssueLabel;
import pt.isel.ls.ResultType.Result;

import java.sql.Connection;
import java.util.Map;

public class DeleteLabelFromIssue implements Command {
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = connectionProvider.getConnection();

        final String PROJECT_ID_KEY = "projectID";
        final String ISSUE_ID_KEY = "IssueID";
        final String LABEL_NAME_KEY = "label-name";


        int issueID = (int) paramMap.get(ISSUE_ID_KEY);
        String labelName = (String) paramMap.get(LABEL_NAME_KEY);
        ProjectDAO projectDAO  = new ProjectDAOImpl(connection);
        String projectName = projectDAO.getSingleProject((int) paramMap.get(PROJECT_ID_KEY)).name;
        IssueLabel issueLabel = new IssueLabel.IssueLabelBuilder().withIssueID(issueID).withLabelDesc(labelName).withProjectName(projectName).build();

        IssueLabelDAO issueLabelDAO = new IssueLabelDAOImpl(connection);
        issueLabelDAO.deleteLabelFromIssue(issueLabel);

        return null;
    }
}
