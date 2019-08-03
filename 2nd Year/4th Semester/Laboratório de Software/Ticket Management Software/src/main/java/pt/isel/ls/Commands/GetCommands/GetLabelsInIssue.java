package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.Issue.IssueDAO;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAO;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAOImpl;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAO;
import pt.isel.ls.DAO.ProjectLabel.ProjectLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.Project;
import pt.isel.ls.Model.ProjectLabel;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetLabelsInIssue;

import java.sql.Connection;
import java.util.LinkedList;
import java.util.Map;

public class GetLabelsInIssue implements Command {
    private static final String PROJECT_ID_KEY = "projectID";
    private static final String ISSUE_ID_KEY   = "issueID";
    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        int projID = (int) paramMap.get(PROJECT_ID_KEY);
        int issueID = (int) paramMap.get(ISSUE_ID_KEY);

        Connection connection = connectionProvider.getConnection();

        ProjectDAO projectDAO = new ProjectDAOImpl(connection);
        Project project = projectDAO.getSingleProject(projID);
        IssueDAO issueDAO = new IssueDAOImpl(connection);
        Issue issue = issueDAO.getSingleIssue(issueID);

        LinkedList<Label> labelList;
        LinkedList<ProjectLabel> projectLabelList;

        if(project.name.equals(issue.projectName)) {
            ProjectLabelDAO projectLabelDAO = new ProjectLabelDAOImpl(connection);
            projectLabelList = projectLabelDAO.getLabelsForProject(project.name);
            IssueLabelDAO issueLabelDAO = new IssueLabelDAOImpl(connection);
            labelList = issueLabelDAO.getLabels(issueID);
        } else
            throw new CommandException("Issue " + issueID + " is not in project " + projID);

        return new ResultGetLabelsInIssue(projID,issueID,projectLabelList,labelList);
    }
}
