package pt.isel.ls.DAO.Issue;

import pt.isel.ls.Exceptions.CommandException;
import pt.isel.ls.Exceptions.InvalidParamException;
import pt.isel.ls.Model.Issue;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public interface IssueDAO {
    List<Issue> getIssueList(int projectId, Map paramMap) throws InvalidParamException;

    int create(Issue i) throws InvalidParamException;

    void update(Issue i, String[] params);

    void delete(Issue i);

    Issue getSingleIssue(int id) throws CommandException;

    void openIssue(Issue i) throws InvalidParamException;
    void closeIssue(Issue i) throws InvalidParamException;

    List<Issue> getIssuesFromProjectWithLabel(int projectID, String labelDesc) throws InvalidParamException;

    boolean isOpen(int issueID) throws SQLException;
}
