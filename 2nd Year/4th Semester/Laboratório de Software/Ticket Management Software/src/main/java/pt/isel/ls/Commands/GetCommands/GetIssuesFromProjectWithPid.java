package pt.isel.ls.Commands.GetCommands;

import pt.isel.ls.Commands.Command;
import pt.isel.ls.Controller.ConnectionProvider;
import pt.isel.ls.Controller.Pair;
import pt.isel.ls.Controller.RequestContext;
import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.DAO.Issue.IssueDAOImpl;
import pt.isel.ls.Model.Issue;
import pt.isel.ls.ResultType.Result;
import pt.isel.ls.ResultType.ResultGet.ResultGetIssues;

import java.util.List;
import java.util.Map;

public class GetIssuesFromProjectWithPid implements Command {

    @Override
    public Result execute(RequestContext requestContext, ConnectionProvider connectionProvider) throws CommandException{
        Map paramMap = requestContext.getParamMap();
        List<Pair<String,String>> labels = null;
        if(requestContext.hasLabels())
            labels = requestContext.getLabels();

        int projectID = (int) paramMap.get("projectID");
        IssueDAOImpl issueDAOImpl;
        issueDAOImpl = new IssueDAOImpl(connectionProvider.getConnection());
        List<Issue> issues;
        issues = issueDAOImpl.getIssueList(projectID,paramMap);
        if(issues.isEmpty())
            throw new CommandException("Project doesn't have any issues");
        ResultGetIssues  res = new ResultGetIssues(issues, projectID);
        res.pid = projectID;
        return res;
    }
}
