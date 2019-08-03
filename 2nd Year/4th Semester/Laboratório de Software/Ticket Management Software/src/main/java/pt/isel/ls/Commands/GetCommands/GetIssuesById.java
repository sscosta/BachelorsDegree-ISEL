package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAO;
import pt.isel.ls.DAO.IssueLabel.IssueLabelDAOImpl;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.Model.Comment;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.Model.Label;
import pt.isel.ls.Model.Project;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.DAO.Project.ProjectDAO;
import pt.isel.ls.DAO.Project.ProjectDAOImpl;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssueById;

import java.sql.Connection;
import java.util.LinkedList;
import java.util.Map;

public class GetIssuesById implements Command {
    private static final String ISSUE_ID_KEY   = "issueID";
    private static final String PROJECT_ID_KEY = "projectID";
    public GetIssuesById() {
    }

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider con) throws CommandException {
        Map paramMap = requestContext.getParamMap();
        Connection connection = con.getConnection();
        int PROJid = (int) paramMap.get(PROJECT_ID_KEY);
        int issueID = (int) paramMap.get(ISSUE_ID_KEY);

        IssueDAOImpl issueDAOImpl;
        issueDAOImpl = new IssueDAOImpl(connection);
        Issue issue;

            issue = issueDAOImpl.getSingleIssue(issueID);
            ProjectDAO projectDAO = new ProjectDAOImpl(connection);
            Project project = projectDAO.getSingleProject(PROJid);
            if(project.name.equals(issue.projectName)) {
                LinkedList<Comment> commentList = issueDAOImpl.getComments(issueID);
                issue.setComments(commentList);
            }
            else
                throw new CommandException("Issue Id not found in This Project");
            //returns labels for the iid issue.
            IssueLabelDAO issueLabelDAO = new IssueLabelDAOImpl(connection);
            LinkedList<Label> labelsInIssue = issueLabelDAO.getLabels(issue.ID);
            if(labelsInIssue!=null)
                issue.setLabels(labelsInIssue);

        return new ResultGetIssueById(issue, PROJid);
    }
}